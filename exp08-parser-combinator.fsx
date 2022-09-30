#r "nuget:FParsec"

open FParsec

type Transaction = Buy | Sell

type Stocks = MSFT | GOOGL | META

type Trade = {
        buyOrSell: Transaction
        ticker: Stocks
        numShares: int
        price: decimal
        allOrNone: bool }

type SharesOf = SharesOf
type At = At
type Portion = AllOrNone | Partial


let convertTransaction inputString = 
  match inputString with
  | "Sell" -> Sell
  | _ -> Buy

let parseTransaction = 
  parse {
    do! spaces
    let! buyOrSellString = choice [(pstring "Buy");(pstring "Sell")]
    return (convertTransaction buyOrSellString)
  } <?> "buyOrSell"

let parseNumShares = 
  parse {
    do! spaces
    let! numShares = pint32
    return numShares
  } <?> "numShares"

let convertTicker inputString = 
  match inputString with
  | "GOOGL" -> GOOGL
  | "META" -> META
  | _ -> MSFT

let parseTicker = 
  parse{
    do! spaces
    let! tickerCharList = (many1 asciiUpper) 
    let tickerString =  tickerCharList |> List.map string |> List.reduce (+)
    let ticker = convertTicker tickerString
    return ticker
  } <?> "ticker"

let parsePrice = 
  parse {
    do! spaces
    let! price = pfloat
    return decimal(price)
  } <?> "price"

let optionalIgnore str = 
  parse {
    do! spaces
    return! skipMany (pstring str)    
  }

let parsePortion = 
  parse {
    do! spaces
    let! portion = choice [(pstring "AllOrNone"); (pstring "Partial"); (preturn "AllOrNone")]
    return portion = "AllOrNone"
  } <?> "portion"

let parseTrade =
  parse {
    let! buyOrSell = parseTransaction
    let! numShares = parseNumShares         
    do! optionalIgnore "SharesOf"    
    let! ticker = parseTicker
    do! optionalIgnore "At"
    let! price = parsePrice
    let! allOrNone = parsePortion 

    return {buyOrSell = buyOrSell; numShares = numShares; ticker = ticker; price = price; allOrNone = allOrNone}
  } <?> "trade"

let result: ParserResult<Trade list,unit> = runParserOnFile (many parseTrade) () "input.txt" System.Text.Encoding.ASCII

let trades: Trade list = 
  match result with
  | Success (x: Trade list,_,_) -> x
  | _ -> []


module Trade = 

  let buyOne order = 
    let totalPrice = order.price * decimal(order.numShares)
    printfn $"You just purchased {order.numShares} shares of {order.ticker} for a total cost of ${totalPrice:N2}." 
    (-1.0m * totalPrice)

  let sellOne order = 
    let totalPrice = order.price * decimal(order.numShares)
    printfn $"You just sold {order.numShares} shares of {order.ticker} for a total earnings of ${totalPrice:N2}." 
    totalPrice

  let tradeOne order =   
    match order.buyOrSell with
    | Buy -> (buyOne order)          
    | Sell -> (sellOne order)

  let tradeMany order = 
    let totalOrderPrice = 
      order
      |> List.map tradeOne
      |> List.sum
    printfn $"-------------------------------------------------------------------"
    match (totalOrderPrice > 0.0m) with
    | true -> printfn $"You just executed a series of trades that earned you ${totalOrderPrice:N2}."
    | false -> printfn $"You just executed a series of trades that cost you ${totalOrderPrice:N2}."


Trade.tradeMany trades
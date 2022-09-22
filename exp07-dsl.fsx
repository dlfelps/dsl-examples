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

module Trade = 

  type TradeBuilder() =

      member _.Yield _ = []

      [<CustomOperation("Buy")>]
      member _.Buy (previous: seq<Trade>, numShares:int, sharesOf:SharesOf, ticker:Stocks, at:At, price:float, allOrNone:Portion) = 
        [yield! previous
         yield {buyOrSell=Buy; ticker=ticker;numShares=numShares;price=decimal(price);allOrNone=(allOrNone=AllOrNone)}]

      [<CustomOperation("Sell")>]
      member _.Sell (previous: seq<Trade>, numShares:int, sharesOf:SharesOf, ticker:Stocks, at:At, price:float, allOrNone:Portion) = 
        [yield! previous
         yield {buyOrSell=Sell; ticker=ticker;numShares=numShares;price=decimal(price);allOrNone=(allOrNone=AllOrNone)}]
         

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

let trade = Trade.TradeBuilder()

let trades = 
  trade{
    Buy 4 SharesOf MSFT At 258.32 AllOrNone
    Sell 3 SharesOf META At 158.71 AllOrNone
    Sell 6 SharesOf GOOGL At 106.08 AllOrNone
  }

Trade.tradeMany trades
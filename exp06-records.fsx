type Transaction = Buy | Sell

type Stocks = MSFT | GOOGL | META

type Trade = {
        buyOrSell: Transaction
        ticker: Stocks
        numShares: int
        price: decimal
        allOrNone: bool }

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

let (trades: Trade list) = 
  [
    {buyOrSell = Buy; ticker = MSFT; numShares = 4; price = 258.32m; allOrNone=true};
    {buyOrSell = Sell; ticker = META; numShares = 3; price = 158.71m; allOrNone=true};
    {buyOrSell = Sell; ticker = GOOGL; numShares = 6; price = 106.08m; allOrNone=true};    
  ]

Trade.tradeMany trades
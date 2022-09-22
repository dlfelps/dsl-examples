type Transaction = Buy | Sell

type Stocks = MSFT | GOOGL | META

type Trade = {
        buyOrSell: Transaction
        ticker: Stocks
        numShares: int
        price: decimal
        allOrNone: bool }

module Trade = 

  let baseTrade = {buyOrSell = Buy; ticker=MSFT; numShares=0; price=0.0m; allOrNone=true}

  type TradeBuilder() =

      member _.Yield _ = baseTrade

      [<CustomOperation("Buy")>]
      member _.Buy (trade: Trade, input: int) = {trade with numShares = input; buyOrSell = Buy}

      [<CustomOperation("Sell")>]
      member _.Sell (trade: Trade, input: int) = {trade with numShares = input; buyOrSell = Sell}

      [<CustomOperation("SharesOf")>]
      member _.Ticker (trade: Trade, input: Stocks) = {trade with ticker = input}

      [<CustomOperation("At")>]
      member _.Price (trade: Trade, input: float) = {trade with price = decimal(input)}

      [<CustomOperation("AllOrNone")>]
      member _.AllOrNone (trade: Trade) = {trade with allOrNone = true}
    
      [<CustomOperation("Partial")>]
      member _.Partial (trade: Trade) = {trade with allOrNone = false}

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
  [
    trade {
      Buy 4
      SharesOf MSFT
      At 258.32
      AllOrNone
    };
    //AllOrNone optional
    trade {
      Sell 3
      SharesOf META
      At 158.71      
    };
    // order-independent
    trade {
      AllOrNone
      At 106.08      
      SharesOf GOOGL
      Sell 6
    };
  ]

Trade.tradeMany trades
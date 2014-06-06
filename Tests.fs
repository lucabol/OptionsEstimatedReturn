module Tests

#if INTERACTIVE
#r @"FSharp.PowerPack.dll"
#endif

open Engine

// TESTS
#if INTERACTIVE
#r @"C:\Program Files (x86)\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\System.Drawing.dll"
//#r @"C:\Users\Luca Bolognese\Documents\Visual Studio 2013\Projects\OptimalF\packages\FSharp.Charting.0.90.6\lib\net40\FSharp.Charting.dll"
#r @"C:\Projects\OptionsExpectedReturns\packages\FSharp.Charting.0.90.6\lib\net40\FSharp.Charting.dll"
open FSharp.Charting
fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart(); "(Chart)")
#endif
open FSharp.Charting

// Some charts
let areaChart (prices:seq<#seq<float>>) =
    prices |> Seq.map (fun prices -> Seq.max prices, Seq.min prices, Seq.average prices, Seq.average prices)
    |> (fun s -> printfn "%A" (Seq.length s);s)
    |> Chart.Stock
let averageChart (prices:seq<#seq<float>>) =
    prices
    |> Seq.map (Seq.average)
    |> (fun s -> printfn "%A" (Seq.length s);s)
    |> Chart.Line
let pathsChart (prices:seq<#seq<float>>) = prices |> Seq.take 1000 |> Seq.map (Chart.FastLine) |> Chart.Combine
let charts (prices:seq<#seq<float>>) =  prices |> averageChart

module internal BS_Works =
    let gbsp = black_scholesG Put  75. 70. 0.5 0.1 0.05 0.35 // 4.087
    let gbsc = black_scholesG Call 75. 70. 0.5 0.1 0.05 0.35 
    let gbsd = - black_scholesG Put 75. 70. 0.5 0.1 0.05 -0.35 // This should be the same as above for put-call supersymmetry

module internal SABR_Works =
    let sab = SABRVolatility 1. 3.692 -0.17691
    let svol1 = sab 1850. 1923. (30. / 365.) 0.1  // 13% reasonable
    let svol2 = sab 1800. 1923. (30. / 365.) 0.1  // 16% reasonable
    let svol3 = sab 1900. 1923. (30. / 365.) 0.1  // 10% reasonable

module internal Covered_Call =
    let coveredCall shares stockValue callValue x = strategy [
                                                                shares, buy stockValue stock
                                                                shares, sell callValue (call x)
                                                             ]

    let cc1 = coveredCall 100. 100. 10. 100.
    let ccExp0 = cc1 150. 0. 0.1 0.3 0.0 BlackScholes // Should be 1000, which is the premium
    let ccExp1 = cc1 100. 0. 0.1 0.3 0.0 BlackScholes // Should be 1000, which is the premium for cal * shares.
    let ccExp2 = cc1 80. 0. 0.1 0.3 0.0 BlackScholes // Should be -1000, lost 2000 on the stock, but gained 1000 of premium
    let ccExp3 = cc1 80. (60. / 365.) 0.0 0.3 0.0 BlackScholes // Should be less than above as the call is not worthless, so buying back would cost
    let ccExp4 = cc1 80. (60. / 365.) 0.0 1.0 0.0 BlackScholes // Should be less than above as more volatility makes the call more expensive

let beta = 0.7 // beta above this seem to create problems for SABR, but it doesn't matter much ...
let yiel = 0.0
let free = 0.0
let shares = 100.
let time = 30. / 365.
                                                   
module internal CommonTest =
    let simC  = {Simulations = 10000; DaysInYear = 365.}
    let esUnd = {InitialPrice = 1923.; Yield = yiel}

    let noStop p v t payoff = false

    let putRatioSpread shares buyPutStrike sell1PutStrike sell2PutStrike buyPrice sell1Price sell2Price =
        strategy [
            1.,   commission 45.
            shares, buy buyPrice (put buyPutStrike)
            shares, sell sell1Price (put sell1PutStrike)
            shares, sell sell2Price (put sell2PutStrike)
        ]

    let fp45 = SABROptionPrice beta 3.692 -0.17691 Put 1845. 1923. time free 0.1 yiel
    let fp30 = SABROptionPrice beta 3.692 -0.17691 Put 1830. 1923. time free 0.1 yiel
    let fp00 = SABROptionPrice beta 3.692 -0.17691 Put 1800. 1923. time free 0.1 yiel

    let esPRS       = putRatioSpread shares 1845. 1830. 1800. fp45 fp30 fp00
    esPRS 1750. 0. 0.01 0.12 0. BlackScholes, esPRS 1776. 0. 0.01 0.12 0. BlackScholes, esPRS 1790. 0. 0.01 0.12 0. BlackScholes, esPRS 1800. 0. 0.01 0.12 0. BlackScholes, esPRS 2000. 0. 0.01 0.12 0.  BlackScholes, esPRS 2000. (20. / 365.) 0.01 0.12 0. BlackScholes

    let sim1  = {
        SimConfig = simC
        PricesGenerator = gaussPricingGeneratorFactory 0. 0.1 free yiel 30 365. // wrong, should take vol/rate from functions
        Strategy = esPRS
        StopFunction = noStop
        Underlying = esUnd
        AtmVol = (constATMVol 0.1)
        FreeRate = constR free
        OptionFormula = BlackScholes
        }   


let strike = 1960.
let fairPrice = black_scholes Call strike 1923. time free 0.1 yiel
let fairBuy = strategy [shares, buy fairPrice (call strike)]
// Increase simulations to 10000 to see a proper straight line
let sim1C = {CommonTest.sim1 with Strategy = fairBuy; SimConfig = {CommonTest.simC with Simulations = 10000} }
let sim1CPaths = generatePayoffPaths sim1C
let sim1CDays = toDailyPayoffs sim1CPaths
sim1CDays |> charts







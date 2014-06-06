#if INTERACTIVE
#r @"FSharp.PowerPack.dll"
#endif

#load "Options.fs"

open Engine
open System

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
    |> Chart.Stock
let averageChart (prices:seq<#seq<float>>) =
    prices
    |> Seq.map (Seq.average)
    |> Chart.Line
let pathsChart (prices:seq<#seq<float>>) = prices |> Seq.take (System.Math.Min(Seq.length prices, 100)) |> Seq.map (Chart.FastLine) |> Chart.Combine
let charts (prices:seq<#seq<float>>) =  Chart.Rows [prices |> areaChart;  prices |> averageChart]  

let noStop p v t payoff = false

module TestPrices =
    lazy
    let prices = allPaths 1000 365 365 100. (Brownian(0., 0.2, 0., 0.06))
    let lastPrices = prices |> Seq.map (Seq.last) |> Seq.map (discount 0.06 1.)
    let initPrice = lastPrices |> Seq.average 
    let stdE = stdErr lastPrices // This is slightly wrong and it should be the the stderr of prices discounted, not just prices
    let lowBound, upBound = initPrice - 1.96 * stdE, initPrice + 1.96 * stdE
    prices |> pathsChart
    ()

module TestBSValue =
    let d, r, y, v, sims, daysInYear = 365, 0.0, 0.0, 0.3, 10000, 365
    let t = float d / float daysInYear
    let test style s x =
        let strat = [ 1., Option(d, style, x, black_scholesG style s x t r (r - y) v) ] , d
        let pricePaths = allPaths sims d daysInYear s (Brownian(0., v, y, r))
        let payoffs = pricePaths |> truncatedPayoffsPaths noStop  strat s r v y (float daysInYear) BlackScholes
        let lastPayoffs = payoffs |> Seq.map (Seq.last) |> Seq.map (discount r t)
        lastPayoffs |> Seq.average, lastPayoffs |> stdErr, payoffs

    test Call 100. 100.
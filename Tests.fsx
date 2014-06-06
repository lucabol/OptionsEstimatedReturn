#if INTERACTIVE
#r @"FSharp.PowerPack.dll"
#endif

#load "OptionsMC.fs"

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
    |> Chart.Stock
let averageChart (prices:seq<#seq<float>>) =
    prices
    |> Seq.map (Seq.average)
    |> Chart.Line
let pathsChart (prices:seq<#seq<float>>) = prices |> Seq.take 1000 |> Seq.map (Chart.FastLine) |> Chart.Combine
let charts (prices:seq<#seq<float>>) =  Chart.Rows [prices |> areaChart;  prices |> averageChart]  

let noStop p v t payoff = false
let beta = 0.7 // beta above this seem to create problems for SABR, but it doesn't matter much ...

let createSim sims initialPrice vol free yiel days strategy stopF formula =
    {
        SimConfig = { Simulations = sims; DaysInYear = 365.}
        PricesGenerator = gaussPricingGeneratorFactory 0. vol free yiel days 365. // wrong, should take vol/rate from functions
        Strategy = strategy
        StopFunction = stopF
        Underlying = { InitialPrice = initialPrice; Yield = yiel}
        AtmVol = (constATMVol 0.1)
        FreeRate = constR free
        OptionFormula = formula
    }

module internal BS_Works =
    let gbsp = black_scholesG Put  75. 70. 0.5 0.1 0.05 0.35 // 4.087
    let gbsc = black_scholesG Call 75. 70. 0.5 0.1 0.05 0.35 
    let gbsd = - black_scholesG Put 75. 70. 0.5 0.1 0.05 -0.35 // This should be the same as above for put-call supersymmetry

module internal SABR_Works =
    let sab = SABRVolatility beta 3.692 -0.17691
    let svol1 = sab 1850. 1923. (30. / 365.) 0.1  // 13% reasonable
    let svol2 = sab 1800. 1923. (30. / 365.) 0.1  // 16% reasonable
    let svol3 = sab 1900. 1923. (30. / 365.) 0.1  // 10% reasonable

    // p. 270 of Complete Guide Option Pricing Models high values of rho don't work (i.e. above 0.75) depending on beta
    let sabt x r = SABRVolatility beta 0.5 r x 100. 0.5 0.3
    let s1 = sabt 70. -0.75 
    let s2 = sabt 70. -0.50 
    let s3 = sabt 70. -0.25 
    let s4 = sabt 70.  0.00 
    let s5 = sabt 70.  0.25 
    let s6 = sabt 70.  0.50 
    let s7 = sabt 70.  0.75 

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
                                                   
module internal Put_Ratio_Spread =
    let yiel = 0.0143
    let free = 0.01
    let shares = 250.
    let time = 30. / 365.
    let margin = 33000.

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
        
    let sim1 = createSim 10000 1923. 0.1 free yiel 30 esPRS noStop BlackScholes

    let sim1Paths = generatePayoffPaths sim1
    let sim1Days = toDailyPayoffs sim1Paths |> toPercentage margin

    //sim1Paths |> pathsChart
    sim1Days |> charts

    let sim2 = {sim1 with OptionFormula = SABR(beta, 3.692, -0.17691) }
    let sim2Paths = generatePayoffPaths sim2
    let sim2Days = toDailyPayoffs sim2Paths |> toPercentage margin

    //sim2Paths |> pathsChart
    sim2Days |> charts

    let esStop p v t payoff = p < 1800.

    let sim3 = {sim2 with StopFunction = esStop}
    let sim3Paths = generatePayoffPaths sim3
    let sim3Days = toDailyPayoffs sim3Paths |> toPercentage margin

    //sim3Paths |> pathsChart
    sim3Days |> charts

module internal FairPriceCall =
    // Buying a call at fair BSM price shouldn't give any profit in BSM simulation.
    let yiel = 0.0143
    let free = 0.01
    let shares = 100.
    let time = 30. / 365.

    let strike = 1960.
    let fairPrice = black_scholes Call strike 1923. time free 0.1 yiel
    let fairBuy = strategy [shares, buy fairPrice (call strike)]
    let sim1C = createSim 10000 1923. 0.1 free yiel 30 fairBuy noStop BlackScholes

    // Increase simulations to 10000 to see a proper straight line
    let sim1CPaths = generatePayoffPaths sim1C
    let sim1CDays = toDailyPayoffs sim1CPaths
    sim1CDays |> charts

    // But if you buy low you make money
    let unfairBuy = strategy [shares, buy (fairPrice * 0.5) (call strike)]
    let sim1C = {sim1C with Strategy = unfairBuy }
    let sim1CPaths = generatePayoffPaths sim1C
    let sim1CDays = toDailyPayoffs sim1CPaths
    sim1CDays |> charts

module internal FairPricePut =
    let yiel = 0.0143
    let free = 0.01
    let shares = 100.
    let time = 30. / 365.

    // Buying a put is fair
    let strike = 1960.
    let fairPrice = black_scholes Put strike 1923. time free 0.1 yiel
    let fairBuy = strategy [shares, buy fairPrice (put strike)]
    let sim1 = createSim 10000 1923. 0.1 free yiel 30 fairBuy noStop BlackScholes

    let sim1C = {sim1 with Strategy = fairBuy }
    let sim1CPaths = generatePayoffPaths sim1C
    let sim1CDays = toDailyPayoffs sim1CPaths

    sim1CDays |> charts

module internal FairPriceSell =
    // Selling is fair too
    let yiel = 0.0143
    let free = 0.01
    let shares = 100.
    let time = 30. / 365.

    let strike = 1960.
    let fairPrice = black_scholes Put strike 1923. time free 0.1 yiel
    let fairBuy = strategy [shares, sell fairPrice (put strike)]
    let sim1 = createSim 10000 1923. 0.1 free yiel 30 fairBuy noStop BlackScholes

    let sim1CPaths = generatePayoffPaths sim1
    let sim1CDays = toDailyPayoffs sim1CPaths

    sim1CDays |> charts

module internal FairStock =
    let yiel = 0.0143
    let free = 0.01
    let shares = 100.
    let time = 30. / 365.
    
    let fairBuy = strategy [shares, buy 1923. stock]
    let sim1 = createSim 10000 1923. 0.1 free yiel 30 fairBuy noStop BlackScholes

    let sim1CPaths = generatePayoffPaths sim1
    let sim1CDays = toDailyPayoffs sim1CPaths

    sim1CDays |> charts

module internal FairPriceSellCC =
    let yiel = 0.0143
    let free = 0.01
    let shares = 100.
    let time = 30. / 365.

    // Combinations of options and stocks are also fair
    let strike = 1960.
    let fairPrice = black_scholes Put strike 1923. time free 0.1 yiel
    let fairBuy = strategy [shares, sell fairPrice (put strike); shares, buy 1923. stock]
    let sim1 = createSim 10000 1923. 0.1 free yiel 30 fairBuy noStop BlackScholes

    let sim1CPaths = generatePayoffPaths sim1
    let sim1CDays = toDailyPayoffs sim1CPaths

    sim1CDays |> charts

module internal SellingPutOnSPXSkewShouldBeProfitable =
    let yiel = 0.0143
    let free = 0.01
    let shares = 250.
    let time = 30. / 365.
    let margin = 33000.

    let strike = 1800.
    let fairPrice = SABROptionPrice beta 3.692 -0.17691 Put strike 1923. time free 0.1 yiel
    let fairBuy = strategy [
                    1.,   commission 45.
                    shares, sell fairPrice (put strike)
    ]
    let sim1 = createSim 10000 1923. 0.1 free yiel 30 fairBuy noStop (SABR(beta, 3.692, -0.17691))

    let sim1CPaths = generatePayoffPaths sim1
    let sim1CDays = toDailyPayoffs sim1CPaths |> toPercentage margin
    sim1CDays |> charts

    // Let's try with a stop at 1800
    let priceStop p v t payoff = p < 1800.
    let sim1C = {sim1 with StopFunction = priceStop}
    let sim1CPaths = generatePayoffPaths sim1C
    let sim1CDays = toDailyPayoffs sim1CPaths |> toPercentage margin 
    //sim1CPaths |> pathsChart
    sim1CDays |> charts

    // Let's try stopping when we start to lose money
    let payoffStop p v t payoff = payoff < 0. && t < (28. / 365.) 
    let sim1C = {sim1C with StopFunction = payoffStop}
    let sim1CPaths = generatePayoffPaths sim1C
    let sim1CDays = toDailyPayoffs sim1CPaths |> toPercentage margin
    //sim1CPaths |> pathsChart
    sim1CDays |> charts

module internal BuyingPutOnSPXSkewShouldNotBeProfitable =
    let yiel = 0.0143
    let free = 0.01
    let shares = 100.
    let time = 30. / 365.

    let strike = 1800.
    let fairPrice = SABROptionPrice beta 3.692 -0.17691 Put strike 1923. time free 0.1 yiel
    let fairBuy = strategy [
                    1.,   commission 45.
                    shares, buy fairPrice (put strike)
    ]
    let sim1 = createSim 10000 1923. 0.1 free yiel 30 fairBuy noStop (SABR(beta, 3.692, -0.17691))

    let sim1CPaths = generatePayoffPaths sim1 |> Seq.cache
    let sim1CDays = toDailyPayoffs sim1CPaths
    //sim1CPaths |> pathsChart
    sim1CDays |> charts

module internal It_is_brownian =
    // positive interest makes it upward sloping, div yield makes it downward sloping
    let pricesGen = gaussPricingGeneratorFactory 0. 0.6 0. 0. 365 365.
    let allPaths  = allPricePaths 10000 100. pricesGen
    allPaths |> toDailyPayoffs |> charts




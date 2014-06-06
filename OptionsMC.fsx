module Engine

open System

// TYPES

/// Various parameters for simulation.
type SimConfig = {Simulations: int; DaysInYear: float}

/// What the underlying looks like.
type Underlying = { InitialPrice: float; Yield: float}

/// initial price -> prices for all days
type PricesGenerator = float -> seq<float>

/// current price -> time in year to exp. -> atm vol
type AtmVol = float -> float -> float

/// time in year to exp. -> risk free interest rate
type FreeRate = float -> float

type Style = Call | Put

/// SABR of beta * volvol * rho
type OptionFormula = BlackScholes | SABR of float * float * float

/// s t r v y op -> payoff
type PayoffFunction = float -> float -> float  -> float -> float -> OptionFormula -> float

/// current price -> current Vol  -> time in year to exp. -> payoff -> should stop
type StopFunction = float -> float -> float -> float -> bool

let roundzero x = if x = 0. then 0.0000001 else x
let roundone  x = if x = 1. then 1.0000001 else x
let nonZero x = if x = 0. then invalidArg "value" "arg can't be zero" 


/// Everything needed to run an MC simulation.
type SimulationConfig = {
    SimConfig : SimConfig
    PricesGenerator: PricesGenerator
    Strategy : PayoffFunction
    StopFunction : StopFunction
    Underlying : Underlying
    AtmVol: AtmVol
    FreeRate: FreeRate
    OptionFormula: OptionFormula
}

// OPTION FORMULAS: for now, just black scholes. Even SABR reduces to black scholes with different vol

/// Cumulative Normal Distribution.
let cnd x =
   nonZero x
   let pow x n = exp (n * log(x) ) 
   let a1 =  0.31938153
   let a2 = -0.356563782
   let a3 =  1.781477937
   let a4 = -1.821255978
   let a5 =  1.330274429
   let pi = 4.0 * atan 1.0
   let l  = abs(x)
   let k  = 1.0 / (1.0 + 0.2316419 * l)
   let w  = ref (1.0-1.0/sqrt(2.0*pi)*exp(-l*l/2.0)*(a1*k+a2*k*k+a3*(pow k 3.0)+a4*(pow k 4.0)+a5*(pow k 5.0)))
   if (x < 0.0) then  w := 1.0 - !w
   !w
 
/// style: either Call or Put
/// s: stock price
/// x: strike price of option
/// t: time to expiration in years
/// r: risk free interest rate
/// v: volatility
/// b: cost of carry
///   b = r for B & S (1973) European no dividend
///   b = r - y Merton (1973) European stock option with continuous dividend
///   b = 0     Black (1976) Future option model
///   b = 0 r = 0 Asay (1982) margined future option model
///   b = r - rf Garman & Kohlhagen (1983) currency option model
let black_scholesG style s x t r b v =
    let t = roundzero t /// at time 0 it is the payoff for the option
    nonZero v; nonZero s; nonZero x;

    let d1 = (log (s / x) + (b + v ** 2. / 2.) * t) / (v * sqrt(t))
    let d2 = d1 - v * sqrt(t)
    match style with
    | Call -> s * exp((b - r) * t) * cnd(d1) - x * exp(-r * t) * cnd(d2)
    | Put  -> x * exp(-r * t) * cnd(-d2) - s * exp((b - r) * t) * cnd(-d1)

/// B & S formula for European option with continuous dividend
let black_scholes style x s t r v (y:float) = black_scholesG style s x t r (r - y) v

/// From "Complete Guide to Option Pricing Formulas" p.268
/// beta: from 1. lognorm to 0. norm
/// volvol: vol of vol
/// rho: correlation of vol with strike price
/// beta is used depending on the instrument (i.e. stock index uses 1.)
/// volvol and rho are calibration parameters observing the existing vol skew 
let SABRVolatility  (beta:float) (volvol:float) (rho:float) (X:float) (F:float) (T:float) (atmvol:float) =
    let t = roundzero T /// at time 0 it is the payoff for the option
    nonZero atmvol; nonZero F; nonZero X;

   
    let beta = if beta = 1. then 1.0001 else beta // hack, excel rounds things differently and don't NaN

    let Log(x)          = Math.Log(x)
    let Abs(x:float)    = Math.Abs(x)
    let Sign(x:float)   = Math.Sign(x)
    let ArcCos(x)       = Math.Acos(x)
    let Cos(x)          = Math.Cos(x)

    let alphaSABR (alpha:float) =
        let mutable dSABR: float array = [|0.;0.;0.|]
        let mutable sabrz = 0.
        let mutable y = 0.
        
        dSABR.[0] <- alpha / ((F * X) ** ((1. - beta) / 2.) * (1. + (((1. - beta) ** 2.) / 24.) * (Log(F / X) ** 2.) + ((1. - beta) ** 4. / 1920.) * (Log(F / X) ** 4.)))

        if Abs(F - X) > 10. ** -8. then
            let sabrz = (volvol / alpha) * (F * X) ** ((1. - beta) / 2.) * Log(F / X)
            y <- (sqrt(1. - 2. * rho * sabrz + sabrz ** 2.) + sabrz - rho) / (1. - rho)
            if Abs(y - 1.) < 10. ** -8. then
                dSABR.[1] <- 1.
            else if y > 0. then
                dSABR.[1] <- sabrz / Log(y)
            else
                dSABR.[1] <- 1.
        else
            dSABR.[1] <- 1.

        dSABR.[2] <- 1. + ((((1. - beta) ** 2. / 24.) * alpha ** 2. / ((F * X) ** (1. - beta)))
                        + 0.25 * rho * beta * volvol * alpha / ((F * X) ** ((1. - beta) / 2.))
                        + (2. - 3. * rho ** 2.) * volvol ** 2. / 24.) * T

        dSABR.[0] * dSABR.[1] * dSABR.[2]
                                  
    let croot cubic quadratic linear constant =
        let mutable roots = [|0.;0.;0.|]
        let mutable CRoot = 0.
        
        let a = quadratic / cubic
        let b = linear / cubic
        let C = constant / cubic
        let Q = (a ** 2. - 3. * b) / 9.
        let r = (2. * a ** 3. - 9. * a * b + 27. * C) / 54.

        if r ** 2. - Q ** 3. >= 0. then
            let capA = - (float (Sign(r))) * (Abs(r) + sqrt(r ** 2. - Q ** 3.)) ** ( 1. / 3.)
            let capB = if capA = 0. then 0. else Q / capA
            CRoot <- capA + capB - a / 3.
        else
            let theta = ArcCos(r / Q ** 1.5)

            roots.[0] <- -2. * sqrt(Q) * Cos(theta / 3.) - a / 3.
            roots.[1] <- -2. * sqrt(Q) * Cos(theta / 3. + 2.0943951023932) - 1. / 3.
            roots.[2] <- -2. * sqrt(Q) * Cos(theta / 3. - 2.0943951023932) - 1. / 3.

            if roots.[0] > 0. then CRoot <- roots.[0]
            else if roots.[1] > 0. then CRoot <- roots.[1]
            else if roots.[2] > 0. then CRoot <- roots.[2]

            if roots.[1] > 0. && roots.[1] < CRoot then CRoot <- roots.[1] 
            if roots.[2] > 0. && roots.[2] < CRoot then CRoot <- roots.[2]
            
        CRoot // return result
            
    let findAlpha = croot
                        ((1. - beta) ** 2. * T / (24. * F ** (2. - 2. * beta)))
                        (0.25 * rho * volvol * beta * T / F ** (1. - beta))
                        (1. + (2. - 3. * rho ** 2.) / 24. * volvol ** 2. * T)
                        (-atmvol * F ** (1. - beta))

    alphaSABR findAlpha

let SABROptionPrice beta volvol rho style x s t r atmvol y =
    let bsVol = SABRVolatility  beta volvol rho x s t atmvol
    black_scholes style x s t r bsVol y

let optionPrice = function
                  | BlackScholes -> black_scholes
                  | SABR(beta, volvol, rho) -> SABROptionPrice beta volvol rho

// STRATEGY COMBINATOR

// Supported contract payoff functions.
let call x          = fun s t r v y op -> ((optionPrice op) Call) x s t r v y
let put x           = fun s t r v y op -> ((optionPrice op) Put) x s t r v y
let stock           = fun s t r v y (op:OptionFormula) -> s
let commission (c:float)    = fun s t r v y (op:OptionFormula) -> -c // Commissions is simply a negative payoff in each day (if you close the strategy).

// Combinators to represents buying and selling of contracts.
let buy (paid:float) f      = fun s t r v y op -> f s t r v y op - paid
let sell (received:float) f = fun s t r v y op -> received - f s t r v y op

// Combinator to create a strategy from multiple leg.
let strategy l      = fun s d r v y op -> Seq.fold (fun state (n,f) -> state + n * f s d r v y op) 0. l

// ATM VOL AND FREE RATE CALCULATOR

/// This calculator doesn't change the atm vol for different atm strike prices or time to expiration, or original vol
let constATMVol c = fun s t -> c

/// This keeps the same risk free rate each day
let constR r = fun t -> r

// DEFINE THE RANDOM WALKS (for now just gaussian)

open System.Threading

/// Allow generation of random numbers in a thread safe way
type MySingleton private () = 
  let rnd = Random()
  static let instance = new ThreadLocal<_>(fun () -> MySingleton())
  static member Instance = instance.Value
  member t.Random = rnd

/// generate a value randomly sampled from gaussian curve with mean and std
/// http://mathworld.wolfram.com/Box-MullerTransformation.html 
let gaussSample mean std =
    let rnd = MySingleton.Instance.Random
    let u1 = rnd.NextDouble()
    let u2 = rnd.NextDouble()
    let randStdNormal = Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2)
    mean + std * randStdNormal

/// standard deviation of a sequence
let stdDev s =
    let avg = Seq.average s
    sqrt (Seq.fold (fun acc elem -> acc + (float elem - avg) ** 2.0 ) 0.0 s / float (Seq.length s))

//[1 .. 10000] |> List.map (fun _ -> gaussSample 0. 0.3) |> stdDev

let annualRetToDaily annualMean daysInYear = (Math.Pow(1. + annualMean, 1./daysInYear) - 1.0)
let annualStdToDaily annualStd  daysInYear = (annualStd / sqrt daysInYear)

/// calculate the drift for the geometric brownian motion
let drift annualRet riskFree divYield annualStd t =
        (annualRet + riskFree - divYield - 0.5 * Math.Pow(annualStd, 2.)) * t

/// daily return conforming to a gaussian process
let gaussDailyReturns mean std = Seq.initInfinite (fun _ -> gaussSample mean std)

/// Create a generator for gaussian prices
let gaussPricingGeneratorFactory yearlyRet yearlyVol riskFree divYield days daysInYear =
    //let driftedMean = yearlyRet + drift riskFree divYield yearlyVol (float days / 365.)
    //let dailyMean   = annualRetToDaily driftedMean daysInYear
    let dailyMean = drift yearlyRet riskFree divYield yearlyVol (1. / 365.) 
    let dailyStd   = annualStdToDaily yearlyVol daysInYear
    fun initialPrice ->
        gaussDailyReturns dailyMean dailyStd
        |> Seq.take (days - 1)
        |> Seq.scan (fun p r -> p * Math.Exp(r)) initialPrice

// MC CORE ROUTINES

/// parallel map
let pmap f xs = 
   seq { for x in xs -> async { return f xs } }
   |> Async.Parallel
   |> Async.RunSynchronously

/// Calculates all possible paths of prices
let allPricePaths sims initialPrice pricesGenerator = [1 .. sims] |> pmap (fun _ -> pricesGenerator initialPrice)

/// Calculates all payoffs for all price paths cutting short the ones stopped out (keeping the payoff at stop time for remaining days)
let truncatedPayoffsPaths shouldStop (strat:PayoffFunction) rF volF y pricesPaths daysInYear optionFormula =
    let totalDays = Seq.length (Seq.head pricesPaths) // Doesn't work for empty pricePaths.
    let foldF (isStopped, stopPayoff:float, i, payoffs) p =
        if isStopped
            then isStopped, stopPayoff, i + 1, stopPayoff::payoffs
            else
                let daysRemaining = totalDays - i
                let t = float daysRemaining / daysInYear
                let v = volF p t
                let r = rF t
                let payoff = strat p t r v y optionFormula
                if shouldStop p v t payoff
                    then true, payoff, i + 1, payoff::payoffs
                    else false, 0., i + 1, payoff::payoffs

    pricesPaths |> Seq.map (Seq.fold foldF (false, 0., 0, []))
                |> Seq.map (fun (_, _, _, payoffs) -> payoffs)
                |> Seq.map (Seq.toList >> List.rev)

 
// TODO: The 3 functions below could be optimized, there is too much conversion between different data structures

/// Transpose a sequence of sequences
let zipn items = items |> Matrix.Generic.ofSeq |> Matrix.Generic.transpose
/// Convert from a matrix to a sequence of arrays
let rows (pm:Matrix<_>) = seq {0 .. pm.NumRows - 1} |> Seq.map (fun i -> pm.Row(i)) |> Seq.map (fun r -> r.ToArray())

// FACADE FUNCTIONS

/// Convert payoff/prices from a sequence of paths to a sequence of daily payoffs,
/// where each element of the sequence is the sequence of payoffs for that day (i.e. day 0, day 1, ... day N 
let toDailyPayoffs payoffPaths = payoffPaths |> zipn |> rows
       
let generatePayoffPaths config =
    let pricePaths = allPricePaths (config.SimConfig.Simulations) (config.Underlying.InitialPrice) (config.PricesGenerator)
    let truncatedPayoffs = truncatedPayoffsPaths config.StopFunction
                                                 config.Strategy
                                                 config.FreeRate
                                                 config.AtmVol
                                                 config.Underlying.Yield
                                                 pricePaths
                                                 config.SimConfig.DaysInYear
                                                 config.OptionFormula
    truncatedPayoffs

let toPercentage (margin:float) payoffs = payoffs |> Seq.map (Seq.map (fun p -> p / margin))
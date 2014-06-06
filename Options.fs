module Engine

open System
open System.Threading

type Style = Call | Put

/// initial mean * initial volatility * yield * free rate
type PriceGenerator = 
| Brownian of float * float * float * float

/// SABR: beta * volvol * rho
type OptionFormula =
| BlackScholes
| SABR of float * float * float 

/// Option: days to expiry * style * strike * initial premium
/// Stock: initial price
type Contract =
| Option of int * Style * float * float 
| Stock of float                        
| Commission

/// quantity * contract
type Position = float  * Contract 

/// position list * days to analyze 
type Strategy = Strategy of Position list * int

/// Allow generation of random numbers in a thread safe way
type MySingleton private () = 
  let rnd = Random()
  static let instance = new ThreadLocal<_>(fun () -> MySingleton())
  static member Instance = instance.Value
  member t.Random = rnd

/// Cumulative Normal Distribution.
let cnd x =
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

/// Low discrepancy random number. Not so useful here as they make the price paths too regular, too low drawdown.
let halton index b =
    let mutable result = 0.
    let mutable f = 1. / b
    let mutable i = index
    while i > 0. do
        result <- result + f * (i % b)
        i <- Math.Floor(i / b)
        f <- f / b
    result

/// Returns a sample from the normal distribution in a thread safe way.
let standardSample () =
    let rnd = MySingleton.Instance.Random
    let u1 = rnd.NextDouble()
    let u2 = rnd.NextDouble()
    Math.Sqrt(-2.0 * Math.Log(u1)) * Math.Sin(2.0 * Math.PI * u2)
    
/// Return infinite series of numbers starting from S assuming daysInYear.
let prices daysInYear S = function
    | Brownian(mean, vol, yiel, free) ->
        let dt = 1. / 365.
        let nudt = (mean + free - yiel - 0.5 * vol ** 2.) * dt
        let sigsdt = vol * sqrt(dt)

        let lnSt = ref (log S)
        
        seq {
            yield S
            while true do
                let epsilon = standardSample () 
                lnSt := !lnSt + nudt + sigsdt * epsilon
                yield exp !lnSt 
        }

/// Parallel map
let pmap f xs = 
   seq { for x in xs -> async { return f xs } }
   |> Async.Parallel
   |> Async.RunSynchronously

/// Return sims paths of stock prices assuming a certain priceGen
let allPaths sims days daysInYear S priceGen =  
    seq {1 .. sims}
    |> Seq.map (fun _ -> prices 365. 100. (Brownian(0., 0.2, 0., 0.06)) |> Seq.take days) 

/// standard deviation of a sequence
let stdDev s =
    let avg = Seq.average s
    sqrt (Seq.fold (fun acc elem -> acc + (float elem - avg) ** 2.0 ) 0.0 s / float (Seq.length s))
    
/// Continuous discounting of a value
let discount annualRate timeInYears value = value * exp(-annualRate * timeInYears)

/// Standard error of a sequence
let stdErr values = stdDev values / sqrt (float (Seq.length values)) 

let roundzero x = if x = 0. then 0.0000001 else x
let roundone  x = if x = 1. then 1.0000001 else x
let nonZero x = if x = 0. then invalidArg "value" "arg can't be zero" 

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

/// Calculate the value of a contract
let optionValue style s x t r v y initalDays initialPremium = function
| BlackScholes -> black_scholesG style s x t r (r - y) v

/// Value 
let contractValue s t r v y formula = function
| Option(d, style, x, prem)  -> optionValue  style s x t r v y d prem formula
| Stock(initialPrice) -> s
| Commission -> 1.

let contractCost s t r v y formula = function
| Option(d, style, x, prem)  -> prem
| Stock(initialPrice) -> initialPrice
| Commission -> 1.

let strategyValue valueF s t r v y formula strategy =
    fst strategy |> Seq.map (fun (q, o) -> valueF s t r v y formula o * q)

/// Calculates all payoffs for all price paths cutting short the ones stopped out (keeping the payoff at stop time for remaining days)
let truncatedPayoffsPaths shouldStop strategy  price rF volF y daysInYear optionFormula pricesPaths =
    let totalDays = snd strategy
    let originalCost = strategy
                       |> strategyValue contractCost price totalDays rF volF y optionFormula
                       |> Seq.sum

    let foldF (isStopped, stopPayoff:float, i, payoffs) p =
        if isStopped
            then isStopped, stopPayoff, i + 1, stopPayoff::payoffs
            else
                let daysRemaining = totalDays - i
                let t = float daysRemaining / daysInYear
                let v = volF // make volF a function in future
                let r = rF 
                let payoff = strategy
                             |> strategyValue contractValue p t r v y optionFormula
                             |> Seq.sum 
                             |> (-) originalCost
                if shouldStop p v t payoff
                    then true, payoff, i + 1, payoff::payoffs
                    else false, 0., i + 1, payoff::payoffs

    pricesPaths |> Seq.map (Seq.fold foldF (false, 0., 0, []))
                |> Seq.map (fun (_, _, _, payoffs) -> payoffs)
                |> Seq.map (Seq.toList >> List.rev)



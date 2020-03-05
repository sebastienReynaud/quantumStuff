module QuHO where

import Bosons
import Numeric.AD
import Data.Complex

prefactor :: Int -> Double
prefactor n' = let n = fromIntegral n' in   (-1)**n /(sqrt (2**n* product [1..n]* sqrt pi))


hermitPol :: Int -> Double ->  Double
hermitPol n x = prefactor n * exp (x^2/2) * ((diffs (\x -> exp (-x^2)) x )!!n)


energyStateToRealSpace :: BosonState -> Double -> Complex Double
energyStateToRealSpace (BosonState n c) x = c * (hermitPol (fromIntegral n) x :+ 0)

--time Evolution hw = 1

evolve :: BosonState -> Double -> BosonState
evolve (BosonState n c) t = BosonState n (exp (0 :+ (fromIntegral n * t)) * c)
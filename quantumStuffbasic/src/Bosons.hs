module Bosons where

import Data.Complex
import Data.List


data BosonState = BosonState { lvl :: Integer,
                               coef :: Complex Double } deriving (Show)

                               
instance Eq BosonState where
    b1 == b2 = lvl b1 == lvl b2 
                               
instance Ord BosonState where
    compare b1 b2 = compare (lvl b1) (lvl b2)                               


aop :: BosonState -> BosonState
aop (BosonState n c) = BosonState (n-1) (sqrt(fromIntegral n) * c)  

aDagop :: BosonState -> BosonState
aDagop (BosonState n c) = BosonState (n+1) (sqrt (fromIntegral n+1) * c) 

--Superposition

type SuperposState =  [BosonState]

addStates :: SuperposState -> SuperposState -> SuperposState
addStates s1 s2 = go (sort $ concat [s1,s2])
                  where go :: SuperposState -> SuperposState
                        go (s : ss: sss) = if s == ss then go (BosonState (lvl s) (coef s + coef ss): sss) else s : go (ss : sss)
                        go s = s 

                        





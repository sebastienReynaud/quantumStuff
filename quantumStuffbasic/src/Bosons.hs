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


addEigenStates :: BosonState -> BosonState -> SuperposState
addEigenStates b1 b2 = addStates [b1] [b2]

addStates :: SuperposState -> SuperposState -> SuperposState
addStates s1 s2 = go (sort $ concat [s1,s2])
                  where go :: SuperposState -> SuperposState
                        go (s : ss: sss) = if s == ss then go (BosonState (lvl s) (coef s + coef ss): sss) else s : go (ss : sss)
                        go s = s 

                        



coherentState :: Complex Double -> SuperposState -- cut at 5 eigenstates
coherentState alpha = take 5 $ iterate recurFun (BosonState 0 (1 :+ 0))
                      where recurFun :: BosonState -> BosonState
                            recurFun (BosonState n c) = BosonState (n+1) (c * alpha / fromIntegral (n+1))

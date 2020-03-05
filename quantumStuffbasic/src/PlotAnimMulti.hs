module PlotAnimMulti where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import QuHO
import Bosons
import Data.Complex


width, height, offset :: Int
width = 300
height = 300
offset = 100
fps = 60

window :: Display
window = InWindow "Quantum harmonic oscillator" (width, height) (offset, offset)


background :: Color
background = black

grid1 :: [Double]
grid1 = [-3.5, -3.25 .. 3.5]

handleKeys :: Event -> SystemState -> SystemState -- press key n to change energy state
-- handleKeys (EventKey (Char '0') Down _ _) s = s {bos = (BosonState 0 (1 :+ 0)) }
-- handleKeys (EventKey (Char '1') Down _ _) s = s {bos = (BosonState 1 (1 :+ 0)) }
-- handleKeys (EventKey (Char '2') Down _ _) s = s {bos = (BosonState 2 (1 :+ 0)) }
-- handleKeys (EventKey (Char '3') Down _ _) s = s {bos = (BosonState 3 (1 :+ 0)) } 
handleKeys _ s = s

update :: Float -> SystemState -> SystemState
update seconds s = evoleT seconds s  

evoleT :: Float -> SystemState -> SystemState --relative time, not absolute! 
evoleT seconds sys = sys {bos = (\x -> evolve  x (realToFrac (1.5*seconds))) <$> b}
                      where b = bos sys
--evolve QHO eigenstate 

data SystemState = SystemState {grid :: [Double], bos :: SuperposState} 
 



initialState :: SystemState
initialState = SystemState grid1 [BosonState 0 (0.9 :+ 0),BosonState 1 (1.2 :+ 0)]


drawLineOfCircles :: [(Double,Complex Double)]  -> [Picture]
drawLineOfCircles l = let realp = (\x -> translate (50 * realToFrac (fst x))  (100 * realToFrac (realPart (snd x) )) (color red $ circleSolid 5)) <$>  l
                          imagp = (\x -> translate (50 * realToFrac (fst x))  (100 * realToFrac (imagPart (snd x) )) (color yellow $ circleSolid 5)) <$>  l
                       in realp ++ imagp   

draw :: SystemState -> Picture
draw (SystemState gr bos ) = Pictures $ drawLineOfCircles $ zip gr vals
                                        where  vals =  sumfp (energyStateToRealSpace <$> bos) gr  
                                               sumfp flist plist = (\x -> sum $ flist <*> [x] ) <$> plist

animateTest  :: IO ()
animateTest = play window background fps initialState draw handleKeys update












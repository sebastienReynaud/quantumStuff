module PlotAnim where

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
window = InWindow "Pong" (width, height) (offset, offset)


background :: Color
background = black

handleKeys :: Event -> SystemState -> SystemState
handleKeys (EventKey (Char 'p') Down _ _) s = s
handleKeys _ s = s

update :: Float -> SystemState -> SystemState
update t s = s {values = zip [-3.5, -3.25 .. 3.5] (energyStateToRealSpace (evolve  (BosonState 1 (1 :+ 0)) (realToFrac t)) <$>  [-3.5, -3.25 .. 3.5])} 


--evolve QHO eigenstate 

data SystemState = SystemState {grid :: [Double],  values :: [(Double,Complex Double)], lvl :: Integer} 
 



initialState :: SystemState
initialState = SystemState [-3.5, -3.25 .. 3.5] (zip [-3.5, -3.25 .. 3.5] (energyStateToRealSpace (BosonState 1 (1 :+ 0) ) <$> [-3.5, -3.25 .. 3.5])) 1

draw :: SystemState -> Picture
draw (SystemState gr vals niv ) = Pictures $ (\x -> translate (50 * realToFrac (fst x))  (50 * realToFrac (realPart (snd x) )) (color yellow $ circleSolid 20)) <$>  vals


animateTest:: IO ()
animateTest = play window background fps initialState draw handleKeys update
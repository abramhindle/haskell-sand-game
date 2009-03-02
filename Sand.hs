import Prelude
import GHC.Num
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Monad
import Particle

maxHeight = 480 :: Int
maxWidth = 640 :: Int

background = "background.png"
tinyBall = "ball10x10.png"


-- data Particle = Dust Int Int | Wall Int Int
data Mousestate = Mouse { left :: Bool,
                          middle :: Bool,
                          right :: Bool,
                          wheelDown :: Bool,
                          wheelUp :: Bool
                        }

defaultMousestate = Mouse { left = False, middle = False, right = False, wheelDown = False, wheelUp = False }

data Gamestate = Game { 
      gUpdate :: Bool,
      gMouse :: Mousestate,
      gParticles :: [Particle]
    }

roundInt = fromIntegral . toInteger . floor 

sand_main = do
  SDL.init [InitEverything]
  setVideoMode maxWidth maxHeight 32 []
  screen <- getVideoSurface
  back <- Image.load background
  ball <- Image.load tinyBall
  blitSurface back Nothing screen Nothing
  SDL.flip screen
  let startState = Game { gUpdate = True, gMouse = defaultMousestate , gParticles = [] }
  let drawParticle (Dust x y) = blitSurface ball Nothing screen (Just (Rect (roundInt x) (roundInt y) 10 10))
  let drawFrame state = do
              blitSurface back Nothing screen Nothing
              Monad.sequence_ (map drawParticle $ gParticles state)
              SDL.flip screen
              return (state{ gUpdate = False })
  let eventLoop state = do               
              state <- drawFrame state
              readEvents (particleStateUpdate state)

      readEvents state = do
                  e <- pollEvent
                  case e of
                    (MouseMotion x y _ _) -> do
                           let rx = fromIntegral x
                           let ry = fromIntegral y
                           let newstate = if (left (gMouse state)) 
                                          then (addParticleToState ButtonLeft rx ry state) 
                                          else state
                           eventLoop newstate
                    (MouseButtonUp x y ButtonLeft) -> do
                           eventLoop (state{ gMouse = ((gMouse state){left = False}) })
                    (MouseButtonDown x y b@ButtonLeft) -> do
                           let rx = fromIntegral x
                           let ry = fromIntegral y
                           eventLoop (addParticleToState b rx ry state)
                    (MouseButtonDown x y b@ButtonRight) -> do
                           let rx = fromIntegral x
                           let ry = fromIntegral y
                           eventLoop (addWall b rx ry state)

                    (KeyDown (Keysym SDLK_ESCAPE _ _)) -> do 
                       print "Quitting"
                       quit
                    Quit -> quit 
                    otherwise -> eventLoop state
                             
  eventLoop startState


genericAddParticleToState p ButtonLeft rx ry state =
    (state{ gUpdate = True, gMouse = mb,  gParticles = particles}) where
        newParticle = p (fromIntegral rx) (fromIntegral ry)
        particles = newParticle : (gParticles state)
        mb = (gMouse state){left = True}

addParticleToState ButtonLeft rx ry state =
    (state{ gUpdate = True, gMouse = mb,  gParticles = particles}) where
        newParticle = Dust (fromIntegral rx) (fromIntegral ry)
        particles = newParticle : (gParticles state)
        mb = (gMouse state){left = True}


    

particleX (Dust x _) = x
particleY (Dust x y) = y
particleXY (Dust x y) = (x,y)

translateParticle x y (Dust x' y') = Dust (x + x') (y + y')

isFixed Wall _ _ = True
isFixed _ = False

particleStateUpdate state = state{gUpdate = True, gParticles = filteredParticles } where
    translatedParticles = map (translateParticle 0 1) (gParticles state)
    filteredParticles = filter (\x -> (particleY x < (fromIntegral maxHeight))) translatedParticles

main = sand_main
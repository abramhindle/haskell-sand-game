import Prelude
import GHC.Num
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Monad

maxHeight = 480
maxWidth = 640

background = "background.png"
tinyBall = "ball10x10.png"

-- eventLoop :: IO ()

data Particle = Dust Int Int 
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


sand_main = do
  SDL.init [InitEverything]
  setVideoMode maxWidth maxHeight 32 []
  screen <- getVideoSurface
  back <- Image.load background
  ball <- Image.load tinyBall
  blitSurface back Nothing screen Nothing
  SDL.flip screen
  let startState = Game { gUpdate = True, gMouse = defaultMousestate , gParticles = [] }
  let drawParticle (Dust x y) = blitSurface ball Nothing screen (Just (Rect x y 10 10))
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
                    (MouseButtonDown x y ButtonLeft) -> do
                           let rx = fromIntegral x
                           let ry = fromIntegral y
                           eventLoop (addParticleToState ButtonLeft rx ry state)

                    (KeyDown (Keysym SDLK_ESCAPE _ _)) -> do 
                       print "Quitting"
                       quit
                    Quit -> quit 
                    otherwise -> eventLoop state
                             
  eventLoop startState


addParticleToState ButtonLeft rx ry state =
    (state{ gUpdate = True, gMouse = mb,  gParticles = particles}) where
        newParticle = Dust rx ry
        particles = newParticle : (gParticles state)
        mb = (gMouse state){left = True}

particleX (Dust x _) = x
particleY (Dust x y) = y
particleXY (Dust x y) = (x,y)

translateParticle x y (Dust x' y') = Dust (x + x') (y + y')

particleStateUpdate state = state{gUpdate = True, gParticles = filteredParticles } where
    translatedParticles = map (translateParticle 0 1) (gParticles state)
    filteredParticles = filter (\x -> (particleY x < maxHeight)) translatedParticles

main = sand_main
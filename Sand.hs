import Prelude
import GHC.Num
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Monad
import Particle
import Data.SpacePart.QuadTree as Quad

maxHeight = 480 :: Int
maxWidth = 640 :: Int

background = "background.png"
tinyBall = "ball10x10.png"
tinyBlock = "block10x10.png"

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
      gParticles :: [Particle],
      gFixed :: [Particle],
      gFixedQuad ::  QuadTree Particle
    }

      

baseGameState = Game { gUpdate = True, gMouse = defaultMousestate, gParticles = [],
                  gFixed = [], gFixedQuad = Quad.empty}

roundInt = fromIntegral . toInteger . floor 

sand_main = do
  SDL.init [InitEverything]
  setVideoMode maxWidth maxHeight 32 []
  screen <- getVideoSurface
  back <- Image.load background
  ball <- Image.load tinyBall
  block <- Image.load tinyBlock

  blitSurface back Nothing screen Nothing
  SDL.flip screen
  let startState = baseGameState
  let drawParticle (Dust x y _ _) = blitSurface ball Nothing screen (Just (Rect (roundInt x) (roundInt y) 10 10))
      drawParticle (Wall x y) = blitSurface block Nothing screen (Just (Rect (roundInt x) (roundInt y) 10 10))
  let drawFrame state = do
              blitSurface back Nothing screen Nothing
              Monad.sequence_ (map drawParticle $ gParticles state)
              Monad.sequence_ (map drawParticle $ gFixed state)
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
                                          else if (right (gMouse state)) 
                                               then addWall ButtonRight rx ry state
                                               else state
                           eventLoop newstate
                    (MouseButtonUp x y b) -> do
                           eventLoop (state{ gMouse = updateMouseUp b (gMouse state) })
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

updateMouseDown ButtonLeft mouse = mouse{ left = True }
updateMouseDown ButtonRight mouse = mouse{ right = True }
updateMouseUp ButtonLeft mouse = mouse{ left = False }
updateMouseUp ButtonRight mouse = mouse{ right = False }
    
addParticleToState b@ButtonLeft rx ry state =
    (state{ gUpdate = True, gMouse = mb,  gParticles = particles}) where
        xvel = if (mod rx 2 == 0) then 0.01 else (-0.01)
        newParticle = Dust (fromIntegral rx) (fromIntegral ry) xvel  0
        particles = newParticle : (gParticles state)
        mb = updateMouseDown b (gMouse state)



addWall button rx ry state = state{ gUpdate = True, 
                                    gFixed = newFixed, 
                                    gFixedQuad = newTree,
                                    gMouse = mb
                                  }
    where newParticle = Wall (fromIntegral rx) (fromIntegral ry)
          newFixed = newParticle : (gFixed state)
          newTree = Quad.insert newParticle (gFixedQuad state)
          mb = updateMouseDown button (gMouse state)





    

particleX (Dust x _ _ _) = x
particleY (Dust x y _ _) = y
particleXY (Dust x y _ _) = (x,y)

translateParticle x y (Dust x' y' vx vy) = Dust (x + x') (y + y') vx vy
pushParticle velx vely (Dust x' y' velx' vely') = Dust (x'+velx) (y'+vely) (velx'+velx) (vely'+vely)

isFixed (Wall _ _) = True
isFixed _ = False

doesParticleCollide state particle = 
    case collidesWith of
      [] -> False
      _  -> True
    where collidesWith = Quad.query (Particle.particleBoundary particle) (gFixedQuad state)


-- No collisions
singleParticleStep (Dust x y velx vely) = Dust (x + velx) (y + vely) velx vely

accelerate ax ay (Dust x y vx vy) = Dust (x + ax + vx) (y + ay + vy) (vx + ax) (ay + vy)
reflect loss (Dust x y vx vy) = Dust (x + rvx) (y + rvy) rvx rvy
    where rvx = -1.0 * vx * loss
          rvy = -1.0 * vy * loss
reflectY loss (Dust x y vx vy) = Dust (x + rvx) (y + rvy) rvx rvy
    where rvx = 1.001 * vx
          rvy = -1.0 * vy * loss


particleStep state d@(Dust x y velx vely) =
    case (doesParticleCollide state newDust) of
      False -> newDust
      True  -> case (collidesLeft, collidesRight) of
                 (False, False) -> reflect 0.9 d
                 (True,False) -> dustRight
                 (False,True) -> dustLeft
                 (True,True) -> reflect 0.9 d
               --  (False,True) -> 
               --  (False,False) -> Dust (x + velx) y velx vely
                                  
    where newDust  = accelerate 0 0.01 d
          dustLeft :: Particle
          dustLeft = accelerate (-1) 0 d
          dustRight = accelerate 1 0 d          
          collidesLeft = doesParticleCollide state dustLeft
          collidesRight = doesParticleCollide state dustRight


particleStateUpdate state = state{gUpdate = True, gParticles = filteredParticles } where
    translatedParticles = map (particleStep state) (gParticles state)
    filteredParticles = filter (\x -> (particleY x < (fromIntegral maxHeight))) translatedParticles

main = sand_main
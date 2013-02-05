import Prelude
import GHC.Num
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Control.Monad.State
import Control.Monad as Monad
import System.Random as Random
import Harbinger

-- The sand game but with random numbers
-- Not as painful as I expected

-- This is the container for the state


type X = OurState

-- Roll between 1 and n
roll :: Int -> IO Int
roll n = getStdRandom (randomR (1,n))

decide :: Int -> Int -> IO Bool
decide n k = do
  v <- roll k
  return (v <= n)

data Sand = None | LightDustGenerator | DustGenerator | LightDust | Dust | Eater | Wall | Acid 
            deriving (Enum,Eq,Ord,Show)

nextSand Acid = None
nextSand x = succ x
prevSand None = Acid
prevSand x = pred x

isFixed Wall = True
isFixed Eater = True
isFixed _ = False

-- Dumb Hack
weight x = fromEnum x

width = 64
height = 48
spriteWidth = 10 :: Int
spriteHeight = 10 :: Int


maxPixelHeight = height * spriteHeight :: Int
maxPixelWidth = width * spriteHeight :: Int

background = "background.png"
tinyBall = "ball10x10.png"
tinyGreenBall = "greenball10x10.png"
tinyBlock = "block10x10.png"
tinyEater = "eater10x10.png"
tinyBlank = "blank10x10.png"
acidBall = "acidball10x10.png"
dustGenSpr = "dustgen10x10.png"
lightdustGenSpr = "lightdustgen10x10.png"

rand :: Int -> Int -> IO Int
rand mi mx = getStdRandom (randomR (mi,mx))

-- btw a room is bottom to top
-- bottom : row above bottom : ... : row above above .. : toprow : []

emptyLine = replicate width None
emptyRoom = Room (replicate height emptyLine)
    


data Room = Room [[Sand]]
data World = World { currentSand :: Sand,
                     room :: Room
                   }



data Cursor = Cursor { 
      pos :: (Int,Int),
      center :: Sand,
      left :: Sand,
      right :: Sand,
      above :: Sand }

data SDLState = SDLState {
      leftButton :: Bool,
      back :: Surface,
      ball :: Surface,
      greenball :: Surface,
      acidball :: Surface,
      block :: Surface,
      eater :: Surface,
      blank :: Surface,
      dustgen :: Surface,
      lightdustgen :: Surface,
      screen :: Surface
    }

data OurState = OurState { harbinger :: Harbinger,
                           sdlstate  :: SDLState
                         }





getCursor pos (center:[]) (above:[]) lx =
    Cursor {pos=pos, center = center, left = lx, right = None, above = above }
getCursor pos (center:right:cline) (above:aline) lx =
    Cursor {pos=pos, center = center, left = lx, right = right, above = above }

sink c@(Cursor { center = center, left = left, right = right, above = above }) =
    c{ center = above, above = center }

bumpLeft c@(Cursor { center = center, left = left, right = right, above = above }) =
    c{ center = left, left = center }

bumpRight c@(Cursor { center = center, left = left, right = right, above = above }) =
    c{ center = right, right = center }

sinkLeft c@(Cursor { center = center, left = left, right = right, above = above }) =
    c { left = above, above = left }

sinkRight c@(Cursor { center = center, left = left, right = right, above = above }) =
    c { right = above, above = right }

eatAbove c = c{above = None }

eatAboveSendMessage c@(Cursor { pos = (x,y), above = above}) = do
    h <- getHarbinger
    let eatstr = (concat ["Eaten at ",(show x)," ",(show y)," ",(show above)])
    v <- io $ harbingerSend h eatstr
    io $ print $ show v
    io $ print eatstr
    return $ eatAbove c

cursorLogic :: Cursor -> StateT X IO Cursor
cursorLogic c@(Cursor { center = Eater, above = Dust }) = eatAboveSendMessage c 
cursorLogic c@(Cursor { center = Eater, above = LightDust }) = eatAboveSendMessage c 
cursorLogic c@(Cursor { center = Eater, above = Acid }) = eatAboveSendMessage c 
cursorLogic c@(Cursor { center = Eater, above = DustGenerator }) = eatAboveSendMessage c 
cursorLogic c@(Cursor { center = Eater, above = LightDustGenerator }) = eatAboveSendMessage c 

cursorLogic c@(Cursor { above = Acid }) =  maybeSink c

cursorLogic c@(Cursor { center = None, above = Dust }) = maybeSink c
cursorLogic c@(Cursor { center = None, above = LightDust }) =  maybeSink c
cursorLogic c@(Cursor { center = None, above = DustGenerator }) =  maybeSink c
cursorLogic c@(Cursor { above = LightDust, center = DustGenerator }) =  maybeSink c
cursorLogic c@(Cursor { center = None, above = LightDustGenerator }) =  maybeSink c
cursorLogic c@(Cursor { above = LightDust, center = LightDustGenerator }) =  maybeSink c



cursorLogic c@(Cursor { center = DustGenerator, above = None }) = return $ c{above = Dust }
cursorLogic c@(Cursor { center = LightDustGenerator, above = None }) = return $ c{above = LightDust }

cursorLogic c@(Cursor { left = None, right = None, above = LightDust }) =  sinkLeftOrRight c

cursorLogic c@(Cursor { left = None, right = None, above = Dust }) =  sinkLeftOrRight c
cursorLogic c@(Cursor { right = None, above = Dust }) = maybeSinkRight c
cursorLogic c@(Cursor { right = None, above = LightDust }) =  maybeSinkRight c

cursorLogic c@(Cursor { center = Dust, left = None , right = None }) = bumpLeftOrRight c
cursorLogic c@(Cursor { center = Dust, left = LightDust , right = LightDust }) = decideOn bumpLeftOrRight c

cursorLogic c@(Cursor { center = LightDust, left = None , right = None }) = bumpLeftOrRight c

cursorLogic c@(Cursor { center = Dust, left = None }) = maybeBumpLeft c
cursorLogic c@(Cursor { center = LightDust, left = None }) = maybeBumpLeft c
cursorLogic c@(Cursor { center = Dust, right = None }) = maybeBumpRight c
cursorLogic c@(Cursor { center = LightDust, right = None }) = maybeBumpRight c
cursorLogic c@(Cursor { center = LightDust, above = Dust }) = maybeSink c


cursorLogic c = return $ c

ourDecision :: StateT X IO Bool
ourDecision = do
    v <- io $ decide 1 2
    return v

-- decideOn :: (Cursor -> StateT Harbinger IO Cursor) -> Cursor -> StateT Harbinger IO Cursor
decideOn f c = do
    v <- ourDecision
    o <- if v 
         then do return c
         else do
           f c
    return o
           
                    
-- sinkLeftOrRight :: Cursor -> StateT Harbinger IO Cursor
sinkLeftOrRight = decisionBranch sinkLeft sinkRight

-- bumpLeftOrRight :: Cursor -> StateT Harbinger IO Cursor
bumpLeftOrRight = decisionBranch bumpLeft bumpRight

-- maybeBumpLeft :: Cursor -> StateT Harbinger IO Cursor
maybeBumpLeft = decisionBranch id bumpLeft

-- maybeBumpRight :: Cursor -> StateT Harbinger IO Cursor
maybeBumpRight = decisionBranch id bumpRight

-- maybeSinkLeft :: Cursor -> StateT Harbinger IO Cursor
maybeSinkLeft = decisionBranch id sinkLeft

-- maybeSinkRight :: Cursor -> StateT Harbinger IO Cursor
maybeSinkRight = decisionBranch id sinkRight


-- maybeSink :: Cursor -> StateT Harbinger IO Cursor
maybeSink = decisionBranch id sink

-- decisionBranch :: (Cursor -> Cursor) -> (Cursor -> Cursor) -> Cursor -> StateT Harbinger IO Cursor
decisionBranch ft ff c = do
    decision <- ourDecision
    return $ if decision 
             then ft c
             else ff c
  
    

-- cursorLogic c@(Cursor { center = center, left = left, right = right, above = above }) =
--                if (not (isFixed above) && weight above >  weight center)
--                 then sink c
--                 else if (not (isFixed right) && weight center > weight right) 
--                      then bumpRight c
--                      else if (not (isFixed left) && weight center > weight left) 
--                           then bumpLeft c
--                           else c

-- handleCursor :: Cursor -> StateT Harbinger IO Cursor 
handleCursor c@(Cursor { center = center, left = left, right = right, above = above }) =
    cursorLogic c
    

--     if (not (isFixed above) && weight above >  weight center)
--     then do 
--       v <- rand 1 2
--       return (if (v == 1) 
--               then sink c
--               else c)
--     else do
--         v <- rand 1 100
--         return (if (v > 33 && not (isFixed right) && weight center > weight right) 
--                 then bumpRight c
--                 else if (not (isFixed left) && weight center > weight left) 
--                      then bumpLeft c
--                      else c)


sand_main = do
  newStdGen
  SDL.init [InitEverything]
  setVideoMode maxPixelWidth maxPixelHeight 32 []
  screen <- getVideoSurface
  back <- Image.load background
  ball <- Image.load tinyBall
  greenball <- Image.load tinyGreenBall
  acidball <- Image.load acidBall
  block <- Image.load tinyBlock
  eater <- Image.load tinyEater
  lightdustgen <- Image.load lightdustGenSpr
  dustgen <- Image.load dustGenSpr
  blank <- Image.load tinyBlank
  let sdlstate = SDLState { leftButton = False, 
                            back = back, 
                            ball = ball, 
                            block = block, 
                            blank = blank, 
                            screen = screen, 
                            greenball = greenball, 
                            acidball = acidball, 
                            eater = eater,
                            lightdustgen = lightdustgen,
                            dustgen = dustgen
                          }
  blitSurface back Nothing screen Nothing
  SDL.flip screen
  let world = World { currentSand = Dust, room = emptyRoom }
  harbinger <- makeDefaultHarbinger "Sand5"
  runStateT  (startWorld world) (OurState { harbinger = harbinger, sdlstate = sdlstate })
  return ()

--  evalState $ do 
--              put harbinger
--              do eventLoop sdlstate world
  
startWorld :: World -> StateT X IO ()
startWorld world = do
   eventLoop world

-- roomIteration world =

drawStep s w = do
  blitSurface (back s) Nothing (screen s) Nothing
  drawWorld s (room w)
  drawElm s (currentSand w) (screen s) 0 0
  SDL.flip (screen s)
  return ()

-- eventLoop :: SDLState -> World ->  IO ()
eventLoop ow = do
  w <-  step ow
  e <- io $ pollEvent
  s <- getSDLState
  case e of
    (MouseMotion x y _ _) -> do
                   let rx = fromIntegral x
                   let ry = fromIntegral y
                   eventLoop (if (leftButton s) 
                                then (w{ room = (insertParticle (room w) (currentSand w) rx ry ) })
                                else w)
    (MouseButtonUp x y ButtonLeft) -> do
                 updateSDLState (s{ leftButton = False })
                 eventLoop  w
    (MouseButtonDown x y b@ButtonRight) -> do
                   eventLoop (w{ currentSand = nextSand (currentSand w) })
    (MouseButtonDown x y b@ButtonWheelDown) -> do
                   eventLoop (w{ currentSand = nextSand (currentSand w) })
    (MouseButtonDown x y b@ButtonWheelUp) -> do
                   eventLoop (w{ currentSand = nextSand (currentSand w) })
    (MouseButtonDown x y b@ButtonLeft) -> do
      let rx = fromIntegral x
      let ry = fromIntegral y
      let newroom = insertParticle (room w) (currentSand w) rx ry 
      updateSDLState s{ leftButton = True }
      eventLoop (w{ room = newroom })

    (KeyDown (Keysym SDLK_ESCAPE _ _)) -> do 
      io $ print "Quitting"
      io $ quit
    Quit -> io $ quit 
    otherwise -> eventLoop w


insertParticle (Room room) sand x y =
    let rx = (x `div` spriteWidth)
        ry = (y `div` spriteHeight )
        xhelper xc [] = error "xhelper not found!"
        xhelper xc (l:ls) = 
            if (xc == rx) 
            then sand : ls
            else l : (xhelper (xc + 1) ls)
        yhelper yc [] = error "xhelper not found!"
        yhelper yc (l:ls) =
            if (yc == ry) 
            then (xhelper 0 l) : ls
            else l : (yhelper (yc - 1) ls)
        newroom = yhelper (height - 1) room
    in (Room newroom)
                              
    


step w = do
  s <- getSDLState
  io $ drawStep s w
  newroom <-  roomIter (room w)
  return (w{ room = newroom })

getSprite s x =  (spriteAcc x) s

spriteAcc None =  blank
spriteAcc LightDust = greenball 
spriteAcc Dust = ball
spriteAcc DustGenerator = dustgen
spriteAcc LightDustGenerator = lightdustgen
spriteAcc Wall = block
spriteAcc Eater = eater
spriteAcc Acid = acidball


drawElm s None  screen i j = return True
drawElm s t  screen i j = blitSurface (getSprite s t) Nothing screen (Just (Rect (spriteWidth*i) (spriteHeight * j) spriteWidth spriteHeight))


drawWorld s (Room room) = do  
  let scr = screen s
  Monad.sequence_ (map 
                   (\(i,row) ->
                    Monad.sequence_ (map 
                                     (\(j,elm) -> drawElm s elm scr j (height - 1 - i))
                                     (zip [0..] row)))
                   (zip [0..] room))
  
    
getHarbinger :: StateT X IO Harbinger
getHarbinger = do
  v <- get
  return (harbinger v)

getSDLState :: StateT X IO SDLState
getSDLState = do
  v <- get
  return (sdlstate v)

updateSDLState s = do
  v <- get
  put (v{ sdlstate = s})




-- Takes 2 rows, processes, returns new changed rows - helps processRow
data Caller = FirstOne | SecondOne | ProcessRow deriving Show
processRow h bottom top = do
  (_:b,_:t) <- processRowHelper (0,h) (None:bottom) (None:top)
  return (b,t)

-- Some bug in here.. show stopper


-- Takes 2 rows, processes, returns new changed rows - helps processRow
processRowHelper :: (Int,Int) -> [Sand] -> [Sand] -> StateT X IO ([Sand],[Sand])

processRowHelper (pos@(x,y)) (b@(bl:bh:[])) (t@(tl:th:[])) = do
    let bs =  [None]
    let ts =  [None] 
    let cursor = getCursor pos (bh:bs) (th:ts) bl
    newCursor <- handleCursor cursor 
    let newLeft = left newCursor
    let newAbove = above newCursor
    let newCenter = center newCursor
    let newRight = right newCursor
    return ((newLeft:newCenter:[]), (tl:newAbove:[]))

processRowHelper (pos@(x,y)) (b@(bl:bh:br:mbs)) (t@(tl:th:tr:mts)) = do
    let (bs,ts) = ((br:mbs),(tr:mts))
    let cursor = Cursor { pos = pos, left = bl, center = bh, right = br, above = th } -- getCursor (bh:bs) (th:ts) bl
    newCursor <- handleCursor cursor 
    let newLeft = left newCursor
    let newAbove = above newCursor
    let newCenter = center newCursor
    let newRight = right newCursor
    (nb,nt) <- processRowHelper (x+1,y) (newCenter:newRight:mbs) (newAbove:tr:mts)
    return ((newLeft:nb) , (tl:nt))         

processRowHelper (x,y) a b = error ("Match failure " ++ (show (x,y))  ++ "> " ++ (show a) ++ " | " ++ (show b))

roomCheck (Room room) = length room == height && helper room where
    helper ([]) = True
    helper (x:xs) = if (length x == width) 
                    then helper xs
                    else (error ("Too short! " ++ (show (length x)) ++ " : " ++ (show x)))

roomIter r@(Room room) = do
  (_ : newroom ) <- helper height (emptyLine : room)
  return (Room newroom)
  where
    helper h (bottom:[]) = do
                         (b,t) <- processRow h bottom emptyLine
                         return (b:[])
    helper h (bottom:top:xs) = do 
                          (b,t) <- processRow h bottom top 
                          h <- helper (h - 1) (t:xs)
                          return (b : h)
  
    
    

io :: IO a -> StateT X IO a
io = liftIO
    
main = sand_main

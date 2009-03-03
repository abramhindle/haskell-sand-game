import Prelude
import GHC.Num
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Monad
import Random


data Sand = None | DustGenerator | LightDust | Dust | Wall | Acid 
            deriving (Enum,Eq,Ord,Show)

nextSand Acid = None
nextSand x = succ x
prevSand None = Acid
prevSand x = pred x

isFixed Wall = True
isFixed _ = False

-- Dumb Hack
weight x = fromEnum x

maxPixelHeight = 480 :: Int
maxPixelWidth = 640 :: Int

height = 48 
width = 64
spriteWidth = 10 :: Int
spriteHeight = 10 :: Int
background = "background.png"
tinyBall = "ball10x10.png"
tinyGreenBall = "greenball10x10.png"
tinyBlock = "block10x10.png"
tinyBlank = "blank10x10.png"
acidBall = "acidball10x10.png"

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



data Cursor = Cursor { center :: Sand,
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
      blank :: Surface,
      screen :: Surface
    }





getCursor (center:[]) (above:[]) lx =
    Cursor { center = center, left = lx, right = None, above = above }
getCursor (center:right:cline) (above:aline) lx =
    Cursor { center = center, left = lx, right = right, above = above }

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



cursorLogic c@(Cursor { above = Acid }) = sink c

cursorLogic c@(Cursor { center = None, above = Dust }) = sink c
cursorLogic c@(Cursor { center = None, above = LightDust }) = sink c
cursorLogic c@(Cursor { center = None, above = DustGenerator }) = sink c
cursorLogic c@(Cursor { above = LightDust, center = DustGenerator }) = sink c


cursorLogic c@(Cursor { center = DustGenerator, above = None }) = c{above = LightDust }

cursorLogic c@(Cursor { left = None, right = None, above = Dust }) = sinkLeft c 
cursorLogic c@(Cursor { left = None, right = None, above = LightDust }) = sinkLeft c
cursorLogic c@(Cursor { right = None, above = Dust }) =  sinkRight c
cursorLogic c@(Cursor { right = None, above = LightDust }) =  sinkRight c


cursorLogic c@(Cursor { center = Dust, left = None }) = bumpLeft c
cursorLogic c@(Cursor { center = Dust, right = None }) = bumpRight c
cursorLogic c@(Cursor { center = LightDust, left = None }) = bumpLeft c
cursorLogic c@(Cursor { center = LightDust, right = None }) = bumpRight c

cursorLogic c@(Cursor { center = LightDust, above = Dust }) = sink c


cursorLogic c@(Cursor { center = center, left = left, right = right, above = above }) =
    c
                    
    

-- cursorLogic c@(Cursor { center = center, left = left, right = right, above = above }) =
--                if (not (isFixed above) && weight above >  weight center)
--                 then sink c
--                 else if (not (isFixed right) && weight center > weight right) 
--                      then bumpRight c
--                      else if (not (isFixed left) && weight center > weight left) 
--                           then bumpLeft c
--                           else c


handleCursor c@(Cursor { center = center, left = left, right = right, above = above }) =
    do let v =  cursorLogic c
       return v

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
  blank <- Image.load tinyBlank
  let sdlstate = SDLState { leftButton = False, back = back, ball = ball, block = block, blank = blank, screen = screen, greenball = greenball, acidball = acidball }
  blitSurface back Nothing screen Nothing
  SDL.flip screen
  let world = World { currentSand = Dust, room = emptyRoom }
  eventLoop sdlstate world
  
-- roomIteration world =

drawStep s w = do
  blitSurface (back s) Nothing (screen s) Nothing
  drawWorld s (room w)
  SDL.flip (screen s)
  return ()

eventLoop s ow = do
  w <- step s ow
  e <- pollEvent
  case e of
    (MouseMotion x y _ _) -> do
                   let rx = fromIntegral x
                   let ry = fromIntegral y
                   eventLoop s (if (leftButton s) 
                                then (w{ room = (insertParticle (room w) (currentSand w) rx ry ) })
                                else w)
    (MouseButtonUp x y ButtonLeft) -> do
                   eventLoop (s{ leftButton = False }) w
    (MouseButtonDown x y b@ButtonRight) -> do
                   eventLoop s (w{ currentSand = nextSand (currentSand w) })
    (MouseButtonDown x y b@ButtonWheelDown) -> do
                   eventLoop s (w{ currentSand = nextSand (currentSand w) })
    (MouseButtonDown x y b@ButtonWheelUp) -> do
                   eventLoop s (w{ currentSand = nextSand (currentSand w) })
    (MouseButtonDown x y b@ButtonLeft) -> do
      let rx = fromIntegral x
      let ry = fromIntegral y
      let newroom = insertParticle (room w) (currentSand w) rx ry 
      eventLoop (s{ leftButton = True }) (w{ room = newroom })

    (KeyDown (Keysym SDLK_ESCAPE _ _)) -> do 
      print "Quitting"
      quit
    Quit -> quit 
    otherwise -> eventLoop s w


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
                              
    



step s w = do
  drawStep s w
  newroom <- roomIter (room w)
  return (w{ room = newroom })

getSprite s None = blank s    
getSprite s LightDust = greenball s
getSprite s Dust = ball s
getSprite s DustGenerator = ball s
getSprite s Wall = block s
getSprite s Acid = acidball s


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
  
    



-- Takes 2 rows, processes, returns new changed rows - helps processRow
data Caller = FirstOne | SecondOne | ProcessRow deriving Show
processRow bottom top = do
  (_:b,_:t) <- processRowHelper ProcessRow (None:bottom) (None:top)
  return (b,t)

-- Some bug in here.. show stopper


-- Takes 2 rows, processes, returns new changed rows - helps processRow
processRowHelper :: Caller -> [Sand] -> [Sand] -> IO ([Sand],[Sand])

processRowHelper caller (b@(bl:bh:[])) (t@(tl:th:[])) = do
    let bs =  [None]
    let ts =  [None] 
    let cursor = getCursor (bh:bs) (th:ts) bl
    newCursor <- handleCursor cursor 
    let newLeft = left newCursor
    let newAbove = above newCursor
    let newCenter = center newCursor
    let newRight = right newCursor
    return ((newLeft:newCenter:[]), (tl:newAbove:[]))

processRowHelper caller (b@(bl:bh:br:mbs)) (t@(tl:th:tr:mts)) = do
    let (bs,ts) = ((br:mbs),(tr:mts))
    let cursor = Cursor { left = bl, center = bh, right = br, above = th } -- getCursor (bh:bs) (th:ts) bl
    newCursor <- handleCursor cursor 
    let newLeft = left newCursor
    let newAbove = above newCursor
    let newCenter = center newCursor
    let newRight = right newCursor
    (nb,nt) <- processRowHelper SecondOne (newCenter:newRight:mbs) (newAbove:tr:mts)
    return ((newLeft:nb) , (tl:nt))         

processRowHelper caller a b = error ("Match failure " ++ (show caller)  ++ "> " ++ (show a) ++ " | " ++ (show b))

roomCheck (Room room) = length room == height && helper room where
    helper ([]) = True
    helper (x:xs) = if (length x == width) 
                    then helper xs
                    else (error ("Too short! " ++ (show (length x)) ++ " : " ++ (show x)))

roomIter r@(Room room) = do
  (_ : newroom ) <- helper (emptyLine : room)
  return (Room newroom)
  where
    helper (bottom:[]) = do
                         (b,t) <- processRow bottom emptyLine
                         return (b:[])
    helper (bottom:top:xs) = do 
                          (b,t) <- processRow bottom top 
                          h <- helper (t:xs)
                          return (b : h)
  
    
    


    
main = sand_main
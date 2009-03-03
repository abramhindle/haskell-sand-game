import Prelude
import GHC.Num
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as Image
import Monad
import Random


data Sand = None | LightDust | Dust | Wall  deriving (Enum,Eq,Ord,Show)

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
tinyBlock = "block10x10.png"
tinyBlank = "blank10x10.png"

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
      back :: Surface,
      ball :: Surface,
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

    

handleCursor c@(Cursor { center = Wall }) = do return c
handleCursor c@(Cursor { center = center, left = left, right = right, above = above }) =
    if (not (isFixed above) && weight above >  weight center)
    then do 
      v <- rand 1 2
      return (if (v == 1) 
              then sink c
              else c)
    else do
        v <- rand 1 100
        return (if (v > 33 && not (isFixed right) && weight center > weight right) 
                then bumpRight c
                else if (not (isFixed left) && weight center > weight left) 
                     then bumpLeft c
                     else c)


sand_main = do
  newStdGen
  SDL.init [InitEverything]
  setVideoMode maxPixelWidth maxPixelHeight 32 []
  screen <- getVideoSurface
  back <- Image.load background
  ball <- Image.load tinyBall
  block <- Image.load tinyBlock
  blank <- Image.load tinyBlank
  let sdlstate = SDLState { back = back, ball = ball, block = block, blank = blank, screen = screen }
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
--     (MouseMotion x y _ _) -> do
--                    let rx = fromIntegral x
--                    let ry = fromIntegral y
--                    let newstate = if (left (gMouse state)) 
--                                  then (addParticleToState ButtonLeft rx ry state) 
--                                  else if (right (gMouse state)) 
--                                       then addWall ButtonRight rx ry state
--                                       else state
--                                            eventLoop newstate
--     (MouseButtonUp x y b) -> do
--                                         eventLoop (state{ gMouse = updateMouseUp b (gMouse state) })
    (MouseButtonDown x y b@ButtonLeft) -> do
      let rx = fromIntegral x
      let ry = fromIntegral y
      let newroom = insertParticle (room w) (currentSand w) rx ry 
      eventLoop s (w{ room = newroom })

    (KeyDown (Keysym SDLK_ESCAPE _ _)) -> do 
      print "Quitting"
      quit
    Quit -> quit 
    otherwise -> eventLoop s w


insertParticle (Room room) sand x y =
    let rx = (x `div` spriteWidth)
        ry = height - 1 - (y `div` spriteHeight )
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
  return w -- (w{ room = newroom })

getSprite s None = blank s    
getSprite s LightDust = ball s
getSprite s Dust = ball s
getSprite s Wall = block s

drawElm s None  screen i j = return True
drawElm s t  screen i j = blitSurface (getSprite s t) Nothing screen (Just (Rect (spriteWidth*i) (spriteHeight * j) spriteWidth spriteHeight))


drawWorld s (Room room) = do  
  let scr = screen s
  Monad.sequence_ (map 
                   (\(i,row) ->
                    Monad.sequence_ (map 
                                     (\(j,elm) -> drawElm s elm scr j i)
                                     (zip [0..] row)))
                   (zip [0..] room))
  
    



data Caller = FirstOne | SecondOne | ProcessRow deriving Show
processRow bottom top = do
  ass <-  if (not (length bottom == length top)) 
          then (error ("processRow: Length unequal " ++ (show bottom) ++ " | " ++ (show top)))
          else return True

  (_:b,_:t) <- processRowHelper ProcessRow (None:bottom) (None:top)
  return (b,t)

-- Some bug in here.. show stopper



processRowHelper :: Caller -> [Sand] -> [Sand] -> IO ([Sand],[Sand])

processRowHelper caller (b@(bl:bh:[])) (t@(tl:th:[])) = do

    let bs =  [None]
    let ts =  [None] 
    let cursor = getCursor (bh:bs) (th:ts) bl
    newCursor <- handleCursor cursor 
    let newLeft = left newCursor
    let newAbove = above newCursor
    let newCenter = center newCursor
    return ((newLeft:newCenter:[]), (tl:newAbove:[]))

processRowHelper caller (b@(bl:bh:mbs)) (t@(tl:th:mts)) = do
    ass <-  if (not (length b == length t)) 
            then (error ("Length unequal " ++ (show b) ++ " | " ++ (show t)))
            else return True

    let (bs,ts) = (mbs,mts)
    let cursor = getCursor (bh:bs) (th:ts) bl
    newCursor <- handleCursor cursor 
    let newLeft = left newCursor
    let newAbove = above newCursor
    let newCenter = center newCursor
    (nb,nt) <- processRowHelper SecondOne (newCenter:mbs) (newAbove:mts)
    return ((newLeft:nb) , (tl:nt))         

processRowHelper caller a b = error ("Match failure " ++ (show caller)  ++ "> " ++ (show a) ++ " | " ++ (show b))

roomCheck (Room room) = length room == height && helper room where
    helper ([]) = True
    helper (x:xs) = if (length x == width) 
                    then helper xs
                    else (error ("Too short! " ++ (show (length x)) ++ " : " ++ (show x)))

roomIter r@(Room room) = do
  v <- return (roomCheck r)
  (_ : newroom ) <- helper (emptyLine : room)
  return (Room newroom)
  where
    helper (bottom:[]) = do
                         ass <-  if (not (length bottom == length emptyLine)) 
                                 then (error ("Last Helper Length unequal " ++ (show bottom) ++ " | " ++ (show emptyLine)))
                                 else return True
                         (b,t) <- processRow bottom emptyLine
                         return (b:[])
    helper (bottom:top:xs) = do 
                          ass <-  if (not (length bottom == length top)) 
                                  then (error ("helper:  Length unequal " ++ (show (length bottom))  ++ " " ++ (show ( length top )) ++ " " ++ (show bottom) ++ " | " ++ (show emptyLine)))
                                  else return True

                          (b,t) <- processRow bottom top 
                          ass <-  if (not (length b == length t) || (not (length t == length top))) 
                                  then (error ("helper after processRow:  Length unequal " ++ (show (length b))  ++ " " ++ (show ( length t))))
                                  else return True

                          h <- helper (t:xs)
                          return (b : h)
  
    
    


    

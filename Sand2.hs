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
spriteWidth = 10
spriteHeight = 10
background = "background.png"
tinyBall = "ball10x10.png"
tinyBlock = "block10x10.png"

rand :: Int -> Int -> IO Int
rand mi mx = getStdRandom (randomR (mi,mx))

-- btw a room is bottom to top
-- bottom : row above bottom : ... : row above above .. : toprow : []

emptyRoom = map (\y -> map (\x -> None) [1..width]) [1..height]

data Room = Room [[Sand]]
data World = World { currentSand :: Sand,
                     room :: Room
                   }

emptyLine = map (\x -> None) [1..width]

data Cursor = Cursor { center :: Sand,
                       left :: Sand,
                       right :: Sand,
                       above :: Sand }

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
  blitSurface back Nothing screen Nothing
  
-- roomIteration world =


processRow bottom top = processRowHelper None:bottom None:top

processRowHelper helper  b@(bl:bh:mbs) t@(tl:th:mts) = do
    let bs = (if (mbs == []) then [None] else bs)
    let ts = if (mts == []) then [None] else bs
    let cursor = getCursor (bh:bs) (th:ts) bl
    newCursor <- handleCursor cursor 
    let newLeft = left newCursor
    let newAbove = above newCursor
    let newCenter = center newCursor
    let (nb,nt) = if (mbs == []) 
                  then ([],[])
                  else do return (processRowHelper bh:mbs th:mts)
    return (newLeft:newCenter:nb , tl:newAbove:nt)

            
            
    

roomIter room = newroom where
    helper (bottom:[])     = b : []
        where (b,t) = processRow bottom emptyLine
    helper (bottom:top:xs) = b : (helper (t:xs))
        where (b,t) = processRow bottom top
    (_ : newroom ) = helper emptyLine : room
    
    


    

import Random
import Monad

rollDice :: IO Int
rollDice = getStdRandom (randomR (-1,1))

scaleRoll x = rollDice >>= (\v -> return (v * x))

main = do
  newStdGen
  Monad.sequence_ (map (\x -> do 
                          v <- rollDice
                          x <- scaleRoll 10
                          print (show v)
                          print (show x))
                   [1..10])

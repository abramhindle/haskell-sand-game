import Random
import Monad

rollDice :: IO Int
rollDice = getStdRandom (randomR (-1,1))


main = do
  newStdGen
  Monad.sequence_ (map (\x -> do 
                          v <- rollDice 
                          print (show v))
                   [1..10])

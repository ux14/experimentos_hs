import Control.Monad.State
import qualified Data.Map as Map

type Value = Int
type Weight = Int
type Size = Int

knapsack :: [(Value,Weight)] -> Weight -> Weight -> State (Map.Map (Weight,Size) Value) Value
knapsack [] currW maxW
 | currW > maxW = return (-1000000000)
 | otherwise = return 0

knapsack p@((vi,wi):pl) currW maxW = 
    do
        dp <- get
        let maybeAns = Map.lookup (currW,length p) dp
        if maybeAns /= Nothing
        then
           let Just ans = maybeAns
           in return ans
        else
           do
               v1 <- knapsack pl currW maxW
               v2 <- knapsack pl (currW + wi) maxW
               newDP <- get
               let ans = (max v1 (vi + v2))
               put $ Map.insert (currW,length p) ans newDP
               return ans

main :: IO ()
main = return ()
import Data.List

sequence' :: (Monad m) => [m a] -> m [a]
sequence' [] = return []
sequence' (l:lr) = do
    x <- l
    fmap (\r -> x:r) (sequence' lr)

sequence'' :: (Eq a) => [[a]] -> [[a]]
sequence'' [] = return []
sequence'' (l:lr) = do
    x <- l
    fmap (\r -> x:r) (sequence'' . map (filter (/= x)) $ lr)
module Fixed where
import Data.Vector as V

fixed :: Eq a => (Vector a -> Vector a) -> Vector a -> Vector a
fixed f xs = let xs' = f xs in 
    if V.and $ V.map (\(a,b) -> a==b) $ V.zip xs xs' then
        xs'
    else
        fixed f xs'

fixedIO :: (Eq a, Show a) => (Vector a -> Vector a) -> Vector a -> IO (Vector a)
fixedIO f xs = do
    let xs' = f xs
    putStrLn $ show xs'
    if V.and $ V.map (\(a,b) -> a==b) $ V.zip xs xs' then
        return xs'
    else
        fixedIO f xs'

fixedN :: Eq a => (Int -> Vector a -> Vector a) -> Vector a -> Vector a
fixedN f xs = fixedNH f xs 0

fixedNH :: Eq a => (Int -> Vector a -> Vector a) -> Vector a -> Int -> Vector a
fixedNH f xs n = let xs' = f n xs in 
    if V.and $ V.map (\(a,b) -> a==b) $ V.zip xs xs' then
        xs'
    else
        fixedNH f xs' (n+1)

module Main where
import Data.Vector as V


--type BArr a = Array Int a

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

bblsIndicesE :: Int -> Vector Int
bblsIndicesE len = V.generate len idx where
    idx :: Int -> Int
    idx 0 = 0
    idx x = if x `mod` 2 == 0 then x-1 else x+1

bblsIndicesO :: Int -> Vector Int
bblsIndicesO len = V.generate len idx where
    idx :: Int -> Int
    idx x = if x `mod` 2 == 0 then x+1 else x-1

-- calculate the indicies
-- we assume the even indicies are the sorting "lines"
bitonicIndices :: Int -> Int -> Int -> Vector Int
bitonicIndices sls dist len = V.generate len idx where
    slc :: Int
    slc = len `div` sls
    idx :: Int -> Int
    idx i = id where
        id = ((i*2) - (i `mod` dist)) `div` 2

swap :: (Eq a, Ord a) => Vector a -> Int -> (a, Int) -> a
swap vect index (elem, other) = let oelem = vect !? other in 
    case oelem of 
        Just oval -> 
            if (index < other && elem > oval) then
                oval
            else if (index > other && elem < oval) then 
                oval
            else 
                elem
        Nothing -> elem 

bblsOddIter :: Ord a => Vector a -> Vector a
bblsOddIter xs = V.imap (swap xs) $ V.zip xs $ bblsIndicesO $ V.length xs

bblsEveIter :: Ord a => Vector a -> Vector a
bblsEveIter xs = V.imap (swap xs) $ V.zip xs $ bblsIndicesE $ V.length xs

bubble :: Ord a => Vector a -> Vector a
bubble = bblsOddIter . bblsEveIter

bubbleSort :: Ord a => Vector a -> Vector a
bubbleSort xs = fixed bubble xs 

bubbleSortIO :: (Ord a, Show a) => Vector a -> IO (Vector a)
bubbleSortIO xs = fixedIO bubble xs

main = putStrLn $ show $ bubbleSort $ fromList [6,5,4,3,2,1]
--main = bubbleSortIO $ fromList [5,4,3,2,1]


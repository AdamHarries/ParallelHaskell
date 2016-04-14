module BubbleSort where
import Fixed
import qualified Data.Vector as V

bblsIndicesE :: Int -> V.Vector Int
bblsIndicesE len = V.generate len idx where
    idx :: Int -> Int
    idx 0 = 0
    idx x = if x `mod` 2 == 0 then x-1 else x+1

bblsIndicesO :: Int -> V.Vector Int
bblsIndicesO len = V.generate len idx where
    idx :: Int -> Int
    idx x = if x `mod` 2 == 0 then x+1 else x-1

bblsNIxs :: Int -> (Int -> V.Vector Int)
bblsNIxs n = if n `mod` 2 == 0 then bblsIndicesE else bblsIndicesO

-- network compare and swap, given a swap index generator and an array
ncas :: (Ord a) => (Int -> V.Vector Int) -> V.Vector a -> V.Vector a
ncas ixf v = V.imap (swap v) $ V.zip v $ ixf $ V.length v where
    -- perform comparison and swap
    swap :: (Eq a, Ord a) => V.Vector a -> Int -> (a, Int) -> a
    swap vect index (elem, other) = let oelem = vect V.!? other in 
        case oelem of 
            Just oval -> 
                case (index < other, elem > oval) of
                    (True, True) -> oval
                    (False, False) -> oval
                    _ -> elem
            Nothing -> elem 

sort :: (Eq a, Ord a) => [(Int -> V.Vector Int)] -> V.Vector a -> V.Vector a
sort ixfs xs = fixed (foldl (.) id $ map ncas ixfs) xs

sortN :: (Eq a, Ord a) => (Int -> (Int -> V.Vector Int)) -> V.Vector a -> V.Vector a
sortN ixff xs = fixedN (ncas . ixff) xs

-- bubble sort with a list of swap generators
bubbleSort :: Ord a => V.Vector a -> V.Vector a
bubbleSort xs = sort [bblsIndicesO, bblsIndicesE] xs

-- bubble sort with a function from n -> swaps
bubbleSortN :: Ord a => V.Vector a -> V.Vector a
bubbleSortN xs = sortN bblsNIxs xs

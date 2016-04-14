module BitonicSort where
import Data.Bits
import Control.Monad
import qualified Data.Vector as V 

-- some definitions, for convenience
type Dim = Int
type Label = Int
type Length = Int
data Direction = Up | Down deriving (Show)

-- given a dimension, and a length, get the connected index for each index
pairs :: Length -> Dim -> V.Vector Label
pairs len dim = V.generate len (pair dim) where
    pair :: Dim -> Label -> Label
    pair dim ix = ix `complementBit` dim

-- calculate the direction of comparison 
-- dim = the dimension we're sorting over
directions :: Length -> Dim -> V.Vector Direction
directions len dim = V.generate len (direction dim) where 
    direction :: Dim -> Label -> Direction
    direction dim ix = case ix `testBit` (dim+1) of
        True -> Down
        False -> Up


-- apply an iteration of a network sorter to a vector
ncas :: (Ord a) => V.Vector Direction -> V.Vector Label -> V.Vector a -> V.Vector a
ncas dirs others vxs = let div = V.zip dirs others in
    V.imap pcas div where
    pcas ix (d, ox) = cas vxs d ix ox
    -- compare and swap in a vector, from the perspective of one element
    -- cas :: the vector -> direction -> Us -> Other -> new value
    cas :: (Ord a) => V.Vector a -> Direction -> Label -> Label -> a
    cas v dir ix ox = let (iv, ov) = (v V.! ix, v V.! ox) in
        case ((ix > ox) == (iv > ov), dir) of -- if the vectors are in order
            (True, Up) -> iv
            (True, Down) -> ov
            (False, Up) -> ov
            (False, Down) -> iv

-- list of dimensions pairs (direction, dist) to apply swaps
iterDims :: [(Dim, Dim)]
iterDims = 
    foldr (++) [] $ 
    map (\i -> (dirDims i) `zip` (swapDims i)) [0..] where
        dirDims i = take (i+1) $ repeat i
        swapDims i = (reverse $ take (i+1) [0..])

bsort :: (Show a, Ord a) => V.Vector a -> V.Vector a
bsort v =
    foldl bsortIter v $ 
    take (iterations vlen) $ 
    iterDims 
    where
        vlen :: Length
        vlen = V.length v
        bsortIter :: (Ord a) => V.Vector a -> (Dim, Dim) -> V.Vector a
        bsortIter v (d, p) = ncas (directions vlen d) (pairs vlen p) v
        iterations :: Length -> Int 
        iterations len = tri (r2 + 1) where
            r2 = ceiling $ logBase 2 (fromIntegral len)
            tri n = n * (n-1) `div` 2



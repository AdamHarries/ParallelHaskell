module BubbleSort where
import Data.Vector as V

bblsIndicesE :: Int -> Vector Int
bblsIndicesE len = V.generate len idx where
    idx :: Int -> Int
    idx 0 = 0
    idx x = if x `mod` 2 == 0 then x-1 else x+1

bblsIndicesO :: Int -> Vector Int
bblsIndicesO len = V.generate len idx where
    idx :: Int -> Int
    idx x = if x `mod` 2 == 0 then x+1 else x-1

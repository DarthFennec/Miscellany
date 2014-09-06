module Huffman (encode, decode) where

import Data.List

data Htree = Leaf Char | Branch Htree Htree deriving (Eq, Ord)

encode :: String -> [Int]
encode s = flatTree t ++ (pack.concatMap (init.encodeChar t)) s
  where t = buildTree s

decode :: [Int] -> String
decode s = let (t, r) = unflatTree s in unfoldr (decodeChar t) (unpack r)

buildTree :: String -> Htree
buildTree = cpile.sort.map (\x -> (length x, Leaf $ head x)).group.sort
  where cpile [(_, x)] = x
        cpile ((r, x):(s, y):xs) = cpile $ insert (r + s, Branch x y) xs

flatTree :: Htree -> [Int]
flatTree (Leaf k) = [fromEnum k]
flatTree (Branch x y) = concat [[255], flatTree x, flatTree y]

unflatTree :: [Int] -> (Htree, [Int])
unflatTree (255:xs) = (Branch s t, zs)
  where (s, ys) = unflatTree xs
        (t, zs) = unflatTree ys
unflatTree (x:xs) = (Leaf $ toEnum x, xs)

encodeChar :: Htree -> Char -> [Bool]
encodeChar (Leaf k) c = if k == c then [False] else []
encodeChar (Branch x y) c = case (encodeChar x c, encodeChar y c) of
  ([], []) -> []
  (xs, []) -> False : xs
  ([], ys) -> True : ys

decodeChar :: Htree -> [Bool] -> Maybe (Char, [Bool])
decodeChar _ [] = Nothing
decodeChar (Leaf k) bs = Just (k, bs)
decodeChar (Branch x y) (b:bs) = decodeChar (if b then y else x) bs

pack :: [Bool] -> [Int]
pack [] = []
pack bs = (sum $ zipWith (\x y -> fromEnum y * 2^x) [0..] b) : pack xs
  where (b, xs) = splitAt 8 bs

unpack :: [Int] -> [Bool]
unpack = concatMap $ take 8.map (toEnum.(`mod` 2)).iterate (`div` 2)

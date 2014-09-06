module Main (main) where

import System.Environment

data Tape a = T [a] a [a]

iT :: (Enum a) => [a] -> a -> [a] -> Tape a
iT l c r = T (l ++ repeat (toEnum 0)) c (r ++ repeat (toEnum 0))

next, prev :: Tape a -> Tape a
next (T xs x (y:ys)) = T (x:xs) y ys
prev (T (x:xs) y ys) = T xs x (y:ys)

jumpnext :: (Eq a, Enum a) => a -> a -> Int -> Tape a -> Tape a
jumpnext s e 0 t@(T _ x _) | x == e = next t
jumpnext s e i t@(T _ x _)
  | x == s    = jumpnext s e (i + 1) (next t)
  | x == e    = jumpnext s e (i - 1) (next t)
  | otherwise = jumpnext s e i (next t)

jumpprev :: (Eq a, Enum a) => a -> a -> Int -> Tape a -> Tape a
jumpprev s e 0 t@(T _ x _) | x == s = next t
jumpprev s e i t@(T _ x _)
  | x == s    = jumpprev s e (i - 1) (prev t)
  | x == e    = jumpprev s e (i + 1) (prev t)
  | otherwise = jumpprev s e i (prev t)

fuck :: Tape Char -> Tape Int -> IO ()
fuck (T _ '\0' _) _ = return ()
fuck t@(T _ '>' _) d = fuck (next t) (next d)
fuck t@(T _ '<' _) d = fuck (next t) (prev d)
fuck t@(T _ '+' _) (T x y z) = fuck (next t) (T x (y + 1) z)
fuck t@(T _ '-' _) (T x y z) = fuck (next t) (T x (y - 1) z)
fuck t@(T _ '[' _) d@(T _ 0 _) = fuck (jumpnext '[' ']' 0 (next t)) d
fuck t@(T _ '[' _) d = fuck (next t) d
fuck t@(T _ ']' _) d@(T _ 0 _) = fuck (next t) d
fuck t@(T _ ']' _) d = fuck (jumpprev '[' ']' 0 (prev t)) d
fuck t@(T _ '.' _) d@(T _ n _) = putChar (toEnum n) >> fuck (next t) d
fuck t@(T _ ',' _) (T x _ z) = getChar >>= insertchar
  where insertchar y = fuck (next t) (T x (fromEnum y) z)
fuck t d = fuck (next t) d

main :: IO ()
main = getArgs >>= readFile.head >>= \(x:xs) -> fuck (iT [] x xs) (iT [] 0 [])

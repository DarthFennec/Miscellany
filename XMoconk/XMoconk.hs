module Main (main) where

import Control.Monad
import Control.Monad.Catch
import Data.Char
import Data.List
import Data.Maybe
import Foreign
import Foreign.C.Types
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.IO
import System.Process
import Text.Read

main :: IO ()
main = do
  d <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a <- internAtom d "TUCKER_TICKER" False
  let q = (proc "conky" []) {std_out = CreatePipe}
  forever $ handleAll (\_ -> return ()) $ do
    withCreateProcess q $ \Nothing (Just h) Nothing _ -> forever $ do
      dat <- hGetLine h
      allocaXEvent $ \e -> do
        setEventType e clientMessage
        pokeByteOff e 32 rw
        pokeByteOff e 40 a
        pokeByteOff e 48 (8 :: CInt)
        let datap = plusPtr e 56 :: Ptr CChar
        mapM_ (uncurry $ pokeElemOff datap) $ zip [0..] $ par dat
        sendEvent d rw False structureNotifyMask e
      sync d False

par :: String -> [CChar]
par = concat.map k.splitat ':'.filter (not.isSpace)
  where
    f x = round $ (100 - 100*2^^(-(x :: Int) `div` 1000) :: Double)
    g (Just c:xs) = cluster (fromIntegral c) $ sort $ catMaybes xs
    g _ = [0]
    k [] = [0]
    k xss@(x:xs)
      | xss == "!" = [255]
      | x == '%' = g $ map readMaybe $ splitat '%' xss
      | x == '*' && all isDigit xs = [min 100 $ f $ read xs]
      | all isDigit xss = [min 100 $ read xss]
      | otherwise = [0]

cluster :: Int -> [CChar] -> [CChar]
cluster c xs = g $ kfix (initvs c xsf) xsf
  where
    xsf = map fromIntegral xs :: [Double]
    g (_, ks) = map (min 100.round.avg) $ maxsplit c $ filter (/= []) ks

maxsplit :: Int -> [[Double]] -> [[Double]]
maxsplit c xs
  | c > length xs = maxsplit c $ csplit xs
  | otherwise = take c xs

csplit :: [[Double]] -> [[Double]]
csplit vs = sp (maximum $ map length vs) vs
  where
    sp _ [] = []
    sp n (x:xs)
      | n == length x = let (y, z) = splitAt (n `div` 2) x in y:z:xs
      | otherwise = x:sp n xs

initvs :: Int -> [Double] -> [Double]
initvs c xs
  | c > length xs = xs
  | otherwise = let n = length xs `div` c in j n $ drop (n `div` 2) xs
  where
    j _ [] = []
    j n (k:ks) = k:j n (drop (n - 1) ks)

kfix :: [Double] -> [Double] -> ([Double], [[Double]])
kfix ms xs
  | ms == ms' = (ms', cs)
  | otherwise = kfix ms' xs
  where (ms', cs) = kmeans ms xs

kmeans :: [Double] -> [Double] -> ([Double], [[Double]])
kmeans ms xs = let gs = assn ms xs in (map avg gs, gs)
  where
    f c1 c2 k = abs (c1 - k) < abs (c2 - k)
    assn (c1:c2:cs) ks = let (n1, n2) = span (f c1 c2) ks in n1:assn (c2:cs) n2
    assn _ ks = [ks]

avg :: [Double] -> Double
avg [] = 0
avg xs = go 0 0 xs
  where
    go t c [] = t/c
    go t c (y:ys) = ((go $! (t + y)) $! (c + 1)) $ ys

splitat :: Char -> String -> [String]
splitat c = map (filter (/= c)).groupBy (\_ x -> x /= c)

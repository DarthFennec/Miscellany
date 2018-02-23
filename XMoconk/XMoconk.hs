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
cluster c xs = g $ kmeans $ h
  where
    xsf = map fromIntegral xs :: [Double]
    f c1 c2 k = abs (c1 - k) < abs (c2 - k)
    g ks = take c $ map (min 100.round) ks ++ repeat 255
    h | c > length xsf = xsf
      | otherwise = let n = length xsf `div` c in j n $ drop (n `div` 2) xsf
    j _ [] = []
    j n (k:ks) = k:j n (drop (n - 1) ks)
    avg [] = 0
    avg x = sum x / fromIntegral (length x)
    assn (c1:c2:cs) ks = n1:assn (c2:cs) n2
      where (n1, n2) = span (f c1 c2) ks
    assn _ ks = [ks]
    kmeans cs = if cs /= cs' then kmeans cs' else cs'
      where cs' = map avg $ assn cs xsf

splitat :: Char -> String -> [String]
splitat c = map (filter (/= c)).groupBy (\_ x -> x /= c)

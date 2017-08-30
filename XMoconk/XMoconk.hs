module Main (main) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.IO
import System.Process
import Foreign
import Foreign.C.Types
import Control.Monad
import Control.Monad.Catch
import Data.List
import Data.Char

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
        mapM_ (uncurry $ pokeElemOff datap) $ zip [0..] $ par $ dat
        sendEvent d rw False structureNotifyMask e
      sync d False

par :: String -> [CChar]
par = map k.filter (/= []).map (filter (/= ':')).groupBy g.filter (not.isSpace)
  where f x = round $ (100 - 100*2^^(-(x :: Int) `div` 1000) :: Double)
        g _ y = y /= ':'
        k [] = 0
        k xss@(x:xs)
          | xss == "!" = 255
          | all isDigit xss = min 100 $ read xss
          | x == '*' && all isDigit xs = min 100 $ f $ read xs
          | otherwise = 0

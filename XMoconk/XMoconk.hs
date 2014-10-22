module Main (main) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign
import Foreign.C.Types
import Control.Monad
import Data.List
import Data.Char

main :: IO ()
main = do
  d <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a <- internAtom d "TUCKER_TICKER" False
  forever $ do
    dat <- getLine
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
  where f x = round $ 100 - 100*2^^(-x `div` 1000)
        g x y = y /= ':'
        k xss@(x:xs)
          | all isDigit xss = min 100 $ read xss
          | x == '*' && all isDigit xs = f $ read xs
          | otherwise = 0

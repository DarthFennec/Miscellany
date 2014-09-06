module Main (main) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign
import Foreign.C.Types
import Control.Monad
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
      pokeByteOff e 48 (16 :: CInt)
      let datap = plusPtr e 56 :: Ptr CShort
      mapM_ (uncurry $ pokeElemOff datap) $ zip [0..] $ map read.words.r $ dat
      sendEvent d rw False structureNotifyMask e
    sync d False

r :: String -> String
r = map $ \c -> if isSpace c || isDigit c then c else '0'

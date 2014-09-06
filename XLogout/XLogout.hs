module Main (main) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Foreign
import Foreign.C.Types
import Control.Monad

main :: IO ()
main = do
  d <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a <- internAtom d "TUCKER_LOGOUT" False
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    pokeByteOff e 32 rw
    pokeByteOff e 40 a
    pokeByteOff e 48 (16 :: CInt)
    sendEvent d rw False structureNotifyMask e
  sync d False

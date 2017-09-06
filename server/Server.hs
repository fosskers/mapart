{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import           Codec.Picture
import           Control.Concurrent (myThreadId)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Proxy
import           Data.Word
import           Geography.MapAlgebra
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.JuicyPixels
import           Servant.Server
import           System.Exit
import           System.Posix.Signals hiding (Handler)

---

type API = Get '[PNG] (Image PixelRGBA8)

api :: Proxy API
api = Proxy

server :: Raster p 512 512 Word8 -> Server API
server = pure . rgba . fmap gray

app :: Raster p 512 512 Word8 -> Application
app = serve api . server

main :: IO ()
main = do
  bytes <- BS.readFile "assets/chatta.tif"
  case fromTiff bytes of
    Nothing -> putStrLn "Failed to read Chatta tif"
    Just (r :| _) -> do
      putStrLn "Listening for requests..."
      tid <- myThreadId
      let h = putStrLn "Shutting down." >> E.throwTo tid ExitSuccess
      installHandler keyboardSignal (Catch h) Nothing
      installHandler softwareTermination (Catch h) Nothing
      W.run 8081 $ app r

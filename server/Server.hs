{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import           Codec.Picture
import           Control.Concurrent (myThreadId)
import qualified Control.Exception as E
import           Data.Aeson
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Lazy as M
import           Data.Proxy
import           Data.Word
import           GHC.Generics
import           Geography.MapAlgebra
import           Lucid
import qualified Network.Wai.Handler.Warp as W
import           Servant.API
import           Servant.HTML.Lucid
import           Servant.JuicyPixels
import           Servant.Server
import           Servant.Utils.StaticFiles (serveDirectoryFileServer)
import           System.Exit
import           System.Posix.Signals hiding (Handler)

---

data Colour = GreenRed | Spectrum | BlueGreen | PurpleYellow | BrownBlue
            deriving (Eq, Ord, Show, Generic, FromJSON)

type API = "colour" :> ReqBody '[JSON] Colour :> Post '[PNG] (Image PixelRGBA8)
  :<|> "assets" :> Raw
  :<|> Get '[HTML] (Html ())

api :: Proxy API
api = Proxy

server :: Raster p 512 512 Word8 -> Server API
server r = pure . render r :<|> serveDirectoryFileServer "assets" :<|> pure page

render :: Raster p 512 512 Word8 -> Colour -> Image PixelRGBA8
render r c = rgba $ classify invisible (colourMap c) r

colourMap :: Colour -> M.Map Word8 PixelRGBA8
colourMap c = (maybe greenRed id $ M.lookup c ramps) [1, 25, 50, 75, 100, 125, 150, 175, 200, 225]
  where ramps = M.fromList [ (GreenRed, greenRed), (Spectrum, spectrum), (BlueGreen, blueGreen)
                           , (PurpleYellow, purpleYellow), (BrownBlue, brownBlue) ]

page :: Html ()
page = html_ $ head_ (title_ "Map Art") >> body_ (script_ [src_ "assets/art.js"] "")

app :: Raster p 512 512 Word8 -> Application
app = serve api . server

main :: IO ()
main = do
  bytes <- BS.readFile "assets/chatta.tif"
  case fromTiff bytes of
    Nothing -> putStrLn "Failed to read Chatta tif"
    Just (r :| _) -> do
      -- Set up safe handlers for SIGTERM and SIGKILL
      tid <- myThreadId
      let h = putStrLn "Shutting down." >> E.throwTo tid ExitSuccess
      installHandler keyboardSignal (Catch h) Nothing
      installHandler softwareTermination (Catch h) Nothing
      -- Get to work
      putStrLn "Listening for requests..."
      W.run 8081 $ app r

{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

import           Codec.Picture
import           Control.Concurrent (myThreadId)
import qualified Control.Exception as E
import qualified Data.ByteString as BS
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Lazy as M
import           Data.Proxy
import           Data.Text (Text)
import           Data.Word
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

type API = "colour" :> Capture "colour" Text :> Get '[PNG] (Image PixelRGBA8)
  :<|> "assets" :> Raw
  :<|> Get '[HTML] (Html ())

server :: Raster p 512 512 Word8 -> Server API
server r = pure . render r :<|> serveDirectoryFileServer "assets" :<|> pure page

render :: Raster p 512 512 Word8 -> Text -> Image PixelRGBA8
render r c = rgba $ classify invisible (colourMap c) r

colourMap :: Text -> M.Map Word8 PixelRGBA8
colourMap c = (maybe greenRed id $ M.lookup c ramps) [1, 25, 50, 75, 100, 125, 150, 175, 200, 225]
  where ramps = M.fromList [ ("greenred", greenRed), ("spectrum", spectrum), ("bluegreen", blueGreen)
                           , ("purpleyellow", purpleYellow), ("brownblue", brownBlue) ]

page :: Html ()
page = html_ $ head_ h >> body_ b
  where b = script_ [src_ "assets/art.js"] "" >> script_ [type_ "text/javascript"] "Elm.Art.fullscreen()"
        h = do
          title_ "Map Art"
          meta_ [charset_ "utf-8"]
          meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1, shrink-to-fit=no"]
          link_ [ rel_ "stylesheet"
                , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css"
                , integrity_ "sha384-rwoIResjU2yc3z8GV/NPeZWAv56rSmLldC3R/AZzGRnGxQQKnKkoFVhFQhNUwEyJ"
                , crossorigin_ "anonymous" ]

app :: Raster p 512 512 Word8 -> Application
app = serve (Proxy :: Proxy API) . server

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

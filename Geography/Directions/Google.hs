module Geography.Directions.Google where

import Geography.Geocoding.Google.Get
import qualified Text.HJson as J
import qualified Data.Map as M
import Network.URI (parseURI)
import Network.HTTP (urlEncodeVars)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)
import Data.List (intersperse)

type DirectionsError = String

data TravelMode = Driving | Walking | Bicycling deriving (Eq)
instance Show TravelMode where show Driving = "driving"; show Walking = "walking"; show Bicycling = "bicycling"
data Avoid = Tolls | Highways deriving (Eq)
instance Show Avoid where show Tolls = "tolls"; show Highways = "highways"
data UnitSystem = Metric | Imperial deriving (Eq)
instance Show UnitSystem where show Metric = "metric"; show Imperial = "imperial"

data DirOptions = DirOptions { travelMode :: TravelMode
                             , avoid :: Maybe Avoid
                             , waypoints :: [String]
                             , alternatives :: Bool
                             , units :: Maybe UnitSystem
                             , regionCode :: Maybe String
                             , sensor :: Bool }
  deriving Show

-- | Convenient set of default options to getDirections
defaultDirOptions = DirOptions Driving Nothing [] False Nothing Nothing False

-- | Find directions from origin to destination using set of options
getDirections :: String -> String -> DirOptions -> IO (Either DirectionsError J.Json)
getDirections orig dest opts = do
  case parseURI (mkDirectionsURL orig dest opts) of
    Nothing  -> return (Left "URL encoding error")
    Just uri -> do
      jstring <- fromMaybe "" `fmap` maybeGet uri
      case J.fromString jstring of
        Left e -> return $ Left (show e)
        Right js
          | getStatus js /= "OK" -> return . Left $ getStatus js
          | otherwise            -> return $ fromMaybe (Left "Malformed JSON") (Just (Right js))

directionsURLFormat = "http://maps.googleapis.com/maps/api/directions/json?%s"
mkDirectionsURL :: String -> String -> DirOptions -> String
mkDirectionsURL orig dest opts =
  printf directionsURLFormat $
    urlEncodeVars ([ ("origin", orig)
                   , ("destination", dest)
                   , ("sensor", showBool (sensor opts))
                   , ("alternatives", showBool (alternatives opts)) ] ++
                   (case avoid opts      of Just av -> [("avoid", show av)]     ; _ -> []) ++
                   (case units opts      of Just us -> [("units", show us)]     ; _ -> []) ++
                   (case regionCode opts of Just rc -> [("regionCode", show rc)]; _ -> []) ++
                   (case waypoints opts  of []      -> []; wp -> [("waypoints", wp2s wp)])
                  )
  where wp2s = concat . intersperse "|"
        showBool True = "true"; showBool False = "false"

getStatus :: J.Json -> String
getStatus (J.JObject top) = fromMaybe "Parse error" $ do
  J.JString s <- M.lookup "status" top
  return s

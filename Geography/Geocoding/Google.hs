-- | Google Geocoding interface. Please read
-- <URL: http://code.google.com/apis/maps/documentation/geocoding/>
module Geography.Geocoding.Google (geoEncode, geoDecode, GeocodeError) where

import qualified Text.HJson as J
import qualified Data.Map as M
import Geography.Geocoding.Google.Get
import Network.URI (parseURI)
import Network.HTTP (urlEncodeVars)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

type GeocodeError = String

-- | Convert an address into a latitude, longitude pair.
geoEncode :: String -> IO (Either GeocodeError (Double, Double))
geoEncode a = do
  case parseURI (mkGeoEncodeURL a) of
    Nothing  -> return (Left "URL encoding error")
    Just uri -> do
      jstring <- fromMaybe "" `fmap` maybeGet uri
      case J.fromString jstring of
        Left e -> return $ Left (show e)
        Right js
          | getStatus js /= "OK" -> return . Left $ getStatus js
          | otherwise            -> return $ fromMaybe (Left "Malformed JSON") (Right `fmap` findLatLon js)

-- | Convert a latitude, longitude pair into a street address.
geoDecode :: (Double, Double) -> IO (Either GeocodeError String)
geoDecode ll = do
  case parseURI (mkGeoDecodeURL ll) of
    Nothing  -> return (Left "URL encoding error")
    Just uri -> do
      jstring <- fromMaybe "" `fmap` maybeGet uri
      case J.fromString jstring of
        Left e -> return $ Left (show e)
        Right js
          | getStatus js /= "OK" -> return . Left $ getStatus js
          | otherwise            -> return $ fromMaybe (Left "Malformed JSON") (Right `fmap` findAddress js)

getStatus :: J.Json -> String
getStatus (J.JObject top) = fromMaybe "Parse error" $ do
  J.JString s <- M.lookup "status" top
  return s

findLatLon :: J.Json -> Maybe (Double, Double)
findLatLon (J.JObject top) = do
  J.JArray res <- M.lookup "results" top
  searchJArray res "geometry" $ \ (J.JObject geo) -> do
    J.JObject loc <- M.lookup "location" geo
    J.JNumber lat <- M.lookup "lat" loc
    J.JNumber lon <- M.lookup "lng" loc
    return (fromRational lat, fromRational lon)

findAddress :: J.Json -> Maybe String
findAddress (J.JObject top) = do
  J.JArray res <- M.lookup "results" top
  searchJArray res "formatted_address" $ \ (J.JString fma) -> return fma

geocodeURLFormat = "http://maps.googleapis.com/maps/api/geocode/json?%s"
mkGeoEncodeURL :: String -> String
mkGeoEncodeURL a =
  printf geocodeURLFormat $ urlEncodeVars [("address", a), ("sensor", "false")]
mkGeoDecodeURL :: (Double, Double) -> String
mkGeoDecodeURL (lat, lng) =
  printf geocodeURLFormat $ urlEncodeVars [("latlng", ll), ("sensor", "false")]
  where ll = show lat ++ "," ++ show lng

searchJArray a name f =
  flip findJust a $ \ x ->
    case x of J.JObject m -> do
                y <- M.lookup name m
                f y
              _ -> Nothing

findJust :: (a -> Maybe b) -> [a] -> Maybe b
findJust f [] = Nothing
findJust f (x:xs) = case f x of
   (y@ (Just _)) -> y
   Nothing -> findJust f xs

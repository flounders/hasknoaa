{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as C
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.Vector (Vector(..))
import qualified Data.Vector as V
import qualified Database.SQLite.Simple as DB
import qualified Options.Applicative as OA
import Network.Wreq
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing
  , getXdgDirectory
  , XdgDirectory(..)
  )
import System.IO (IOMode(..), withFile)
import System.IO.Error (isDoesNotExistError, tryIOError)
import Text.Printf

data Units = USC
           | SI
           deriving Show

data WeatherRequest = MultiDayForecast
                    | HourlyForecast
                    | AddPoint Double Double
                    | ListPoints
                    | SetDefaultPoint Int
                    deriving Show

data AppOptions = AppOptions
  { units :: Units
  , weatherRequest :: WeatherRequest
  } deriving Show

appOptions :: OA.Parser AppOptions
appOptions =
  AppOptions
  <$> OA.flag USC SI (OA.long "metric" <> OA.short 'm' <> OA.help "Use SI derived units")
  <*> OA.subparser (OA.command "add" (OA.info addPoint (OA.progDesc "Add a coordinate point for weather data")) <>
                    OA.command "hourly" (OA.info (pure HourlyForecast) (OA.progDesc "Display hourly forecast")) <>
                    OA.command "multiday" (OA.info (pure MultiDayForecast) (OA.progDesc "Display multi-day forecast")) <>
                    OA.command "set" (OA.info setPoint (OA.progDesc "Set default point for forecasts")) <>
                    OA.command "list" (OA.info (pure ListPoints) (OA.progDesc "List saved points"))
                   )
   where addPoint = AddPoint <$> OA.argument OA.auto (OA.metavar "LATITUDE") <*> OA.argument OA.auto (OA.metavar "LONGITUDE")
         setPoint = SetDefaultPoint <$> OA.argument OA.auto (OA.metavar "POINT_ID")

data Point = Point
  { latitude :: Double
  , longitude :: Double
  , forecastUri :: Text
  , forecastHourlyUri :: Text
  , city :: Text
  , state :: Text
  } deriving Show

data Period = Period
  { periodId :: Int
  , startTime :: UTCTime
  , endTime :: UTCTime
  , temperature :: Int
  , temperatureUnit :: Text
  , windSpeed :: Text
  , windDirection :: Text
  , shortForecast :: Text
  , detailedForecast :: Text
  } deriving Show

data Forecast = Forecast
  { updateTime :: Text
  , periods :: Vector Period
  } deriving Show

toPoint :: Maybe A.Value -> Maybe Point
toPoint v = do
  body <- v
  la <- body ^? key "geometry" . key "coordinates" . nth 1 . _Double
  lo <- body ^? key "geometry" . key "coordinates" . nth 0 . _Double
  fu <- body ^? key "properties" . key "forecast" . _String
  fhu <- body ^? key "properties" . key "forecastHourly" . _String
  c <- body ^? key "properties" . key "relativeLocation" . key "properties" . key "city" . _String
  s <- body ^? key "properties" . key "relativeLocation" . key "properties" . key "state" . _String
  pure Point { latitude = la
             , longitude = lo
             , forecastUri = fu
             , forecastHourlyUri = fhu
             , city = c
             , state = s
             }

toPeriod :: A.Value -> Maybe Period
toPeriod v = do
  i <- v ^? key "number" . _Integral
  sTime <- v ^? key "startTime" ._String
  eTime <- v ^? key "endTime" ._String
  t <- v ^? key "temperature" . _Integral
  tUnit <- v ^? key "temperatureUnit" . _String
  wSpeed <- v ^? key "windSpeed" . _String
  wDir <- v ^? key "windDirection" . _String
  short <- v ^? key "shortForecast" . _String
  detail <- v ^? key "detailedForecast" . _String
  sTime' <- iso8601ParseM $ T.unpack sTime
  eTime' <- iso8601ParseM $ T.unpack eTime
  pure Period { periodId = i
              , startTime = zonedTimeToUTC sTime'
              , endTime = zonedTimeToUTC eTime'
              , temperature = t
              , temperatureUnit = tUnit
              , windSpeed = wSpeed
              , windDirection = wDir
              , shortForecast = short
              , detailedForecast = detail
              }

toForecast :: Maybe A.Value -> Maybe Forecast
toForecast v = do
  body <- v
  ut <- body ^? key "properties" . key "updateTime" . _String
  ps <- body ^? key "properties" . key "periods" . _Array
  ps' <- V.mapM toPeriod ps
  pure Forecast { updateTime = ut
                , periods = ps'
                }

getPointFromApi :: Double
                -> Double
                -> IO (Maybe Point, String)
getPointFromApi lat long = do
  let reqOpts = defaults & header "content-type" .~ ["application/geo+json"]
  let f x = showFFloat (Just 4) x ""
  resp <- getWith reqOpts ("https://api.weather.gov/points/" <> f lat <> "," <> f long)
  let body = C.unpack . C.toStrict $ resp ^. responseBody
  pure (decodePoint body, body)

getPointFromCache :: DB.Connection
                  -> Double
                  -> Double
                  -> IO (Maybe Point)
getPointFromCache conn lat long = do
  r <- DB.queryNamed conn "SELECT last_body FROM points WHERE lat = :lat AND long = :long" [":lat" DB.:= lat, ":long" DB.:= long] :: IO [DB.Only String]
  pure $ foldr (\(DB.Only x) acc -> decodePoint x) Nothing r

getPoint :: DB.Connection
         -> Double
         -> Double
         -> IO (Maybe Point)
getPoint conn lat long = do
  p <- getPointFromCache conn lat long
  case p of
    Just _ -> pure p
    Nothing -> do
      (p', b) <- getPointFromApi lat long
      case p' of
        Just x -> do
          DB.execute conn "INSERT INTO points (lat, long, city, state, last_body) VALUES\
            \ (?, ?, ?, ?, ?)" (lat, long, city x, state x, b)
          pure p'
        Nothing -> pure Nothing

getForecastFromApi :: Text
                   -> Units
                   -> IO (Maybe Forecast, String)
getForecastFromApi uri u = do
  let u' = case u of
             USC -> "us"
             SI -> "si"
  let reqOpts = defaults & header "content-type" .~ ["application/geo+json"] & param "units" .~ [u']
  resp <- getWith reqOpts (T.unpack uri)
  let body = C.unpack . C.toStrict $ resp ^. responseBody
  pure (decodeForecast body, body)

getForecastFromCache :: DB.Connection
                     -> Text
                     -> IO (Maybe Forecast)
getForecastFromCache conn uri = do
  r <- DB.query conn "SELECT last_body FROM forecasts WHERE url = ?" (DB.Only uri) :: IO [DB.Only String]
  pure $ foldr (\(DB.Only x) acc -> decodeForecast x) Nothing r

getForecast :: DB.Connection
            -> Text
            -> Units
            -> IO (Maybe Forecast)
getForecast conn uri u = do
  fc <- getForecastFromCache conn uri
  case fc of
    Just x -> do
      p1 <- V.indexM (periods x) 0
      now <- getCurrentTime
      if endTime p1 < now
         then do
           (fc', b) <- getForecastFromApi uri u
           case fc' of
             Just _ -> do
               DB.execute conn "UPDATE forecasts SET last_body = (?) WHERE url = (?)" (b, uri)
               pure fc'
             Nothing -> pure Nothing
         else pure fc
    Nothing -> do
      (fc', b) <- getForecastFromApi uri u
      case fc' of
        Just _ -> do
          DB.execute conn "INSERT INTO forecasts (url, last_body) VALUES (?, ?)" (uri, b)
          pure fc'
        Nothing -> pure Nothing

decodePoint :: String
            -> Maybe Point
decodePoint = toPoint . A.decode . C.fromStrict . C.pack

decodeForecast :: String
               -> Maybe Forecast
decodeForecast = toForecast . A.decode . C.fromStrict . C.pack

initDB :: IO DB.Connection
initDB = do
  p <- getXdgDirectory XdgData "hasknoaa"
  createDirectoryIfMissing True p
  conn <- DB.open $ p <> "/database.sqlite"
  DB.execute_ conn "CREATE TABLE IF NOT EXISTS forecasts (id INTEGER PRIMARY KEY, url TEXT UNIQUE, last_body TEXT)"
  DB.execute_ conn "CREATE TABLE IF NOT EXISTS points (id INTEGER PRIMARY KEY, lat REAL, long REAL, city TEXT,\
    \ state TEXT, last_body TEXT)"
  DB.execute_ conn "CREATE TABLE IF NOT EXISTS defaultPoint (id INTEGER PRIMARY KEY, pointId INTEGER NOT NULL,\
    \ FOREIGN KEY (pointId) REFERENCES points (id))"
  pure conn

showForecast :: Forecast
             -> IO ()
showForecast x = do
  TIO.putStrLn $ "Last updated at: " <> updateTime x
  mapM_ showPeriod (periods x)

showPeriod :: Period
           -> IO ()
showPeriod x = do
  putStrLn $ "Period #" <> show (periodId x)
  putStrLn $ "Start: " <> iso8601Show (startTime x)
  putStrLn $ "End: " <> iso8601Show (endTime x)
  putStr $ "Temperature: " <> show (temperature x)
  TIO.putStrLn $ " " <> temperatureUnit x
  TIO.putStrLn $ "Wind: " <> windSpeed x <> " " <> windDirection x
  TIO.putStrLn $ "Short forecast: " <> shortForecast x
  TIO.putStrLn $ "Detailed forecast: " <> detailedForecast x
  putStrLn ""

main :: IO ()
main = do
  conn <- initDB
  let opts = OA.info (appOptions OA.<**> OA.helper)
              ( OA.fullDesc
              <> OA.progDesc "Interact with the weather.gov API"
              <> OA.header "haskNoaa - A command line weather client for weather.gov"
              )
  appOpts <- OA.execParser opts
  act appOpts conn

act :: AppOptions
    -> DB.Connection
    -> IO ()
act (AppOptions _ (AddPoint lat long)) conn = do
  p <- getPoint conn lat long
  case p of
    Just x -> putStrLn $ "Point is now available: " ++ show x
    Nothing -> putStrLn "Error occurred while trying to get point data"
act (AppOptions _ ListPoints) conn = do
  putStrLn "id | latitude | longitude | city | state"
  rs <- DB.query_ conn "SELECT id, lat, long, city, state FROM points" :: IO [(Int, Double, Double, String, String)]
  mapM_ (\(i, lat, long, city, state) -> printf "%d | %g | %g | %s | %s\n" i lat long city state) rs
act (AppOptions _ (SetDefaultPoint i)) conn = do
  DB.execute_ conn "DELETE FROM defaultPoint"
  -- If the pointId below does not exist, this will hard crash
  DB.execute conn "INSERT INTO defaultPoint (pointId) VALUES (?)" (DB.Only i)
act (AppOptions u MultiDayForecast) conn = do
  is <- DB.query_ conn "SELECT pointId FROM defaultPoint"
  let i = (\(DB.Only x) -> x) $ head is :: Int
  rs <- DB.query conn "SELECT lat, long FROM points WHERE id = (?)" (DB.Only i)
  let (pLat, pLong) = head rs
  p <- getPoint conn pLat pLong
  case p of
    Just x -> do
      f <- getForecast conn (forecastUri x) u
      case f of
        Just y -> showForecast y
        Nothing -> putStrLn "Error: Failed to get forecast"
    Nothing -> putStrLn "Error: Failed to get point for forecast data"
act (AppOptions u HourlyForecast) conn = do
  is <- DB.query_ conn "SELECT pointId FROM defaultPoint"
  let i = (\(DB.Only x) -> x) $ head is :: Int
  rs <- DB.query conn "SELECT lat, long FROM points WHERE id = (?)" (DB.Only i)
  let (pLat, pLong) = head rs
  p <- getPoint conn pLat pLong
  case p of
    Just x -> do
      f <- getForecast conn (forecastHourlyUri x) u
      case f of
        Just y -> showForecast y
        Nothing -> putStrLn "Error: Failed to get forecast"
    Nothing -> putStrLn "Error: Failed to get point for forecast data"

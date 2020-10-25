{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Q.Stats.TimeSeries where
import Data.Time
import qualified Data.Map as M
import Data.Vector (Vector, toList)
import qualified Data.Csv as Csv
import Data.Csv ((.:))
import qualified Data.ByteString.Lazy as B
import GHC.Generics (Generic)
import Data.Time.Format
import Data.Time.Format.ISO8601
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Data.Maybe (fromJust)

-- A single data point with a time and value.
data DataPoint a b = DataPoint {
    dpT :: a  -- ^Time
  , dpV :: b  -- ^Value
  } deriving (Generic, Show, Eq, Ord)

{-|
Read a a csv row with 2 columns: `date,value` where `date` is
in shortened iso format. (with our without time)
-}
instance Csv.FromNamedRecord (DataPoint LocalTime Double) where
  parseNamedRecord m = DataPoint
      <$> fmap (fromJust . parseDateTime) (m .: "date")
      <*> (m .: "value")

{-|
Read a a csv row with 2 columns: `date,value` where `date` is
in year fractions.
-}
instance Csv.FromNamedRecord (DataPoint Double Double) where
  parseNamedRecord m = DataPoint
      <$> (m .: "date")
      <*> (m .: "value")


parseDateTime :: String -> Maybe LocalTime
parseDateTime iso_datetime =
  if length iso_datetime == 8 then
    parseDay iso_datetime
  else                
    formatParseM localTimeFormat' iso_datetime 

localTimeFormat' = (localTimeFormat (calendarFormat BasicFormat) (timeOfDayFormat BasicFormat))
dayFormat' = (calendarFormat BasicFormat)

parseTime :: String -> Maybe LocalTime
parseTime iso_datetime = formatParseM localTimeFormat' iso_datetime

parseDay :: String -> Maybe LocalTime
parseDay iso_date = do
  day <- formatParseM dayFormat' iso_date
  return $ LocalTime day midnight

dateToString :: LocalTime -> String 
dateToString date = formatShow (localTimeFormat (calendarFormat BasicFormat) (timeOfDayFormat BasicFormat)) date

read :: forall a. (Csv.FromNamedRecord a) => FilePath -> IO [a]
read f = do
  s <- (B.readFile f)
  let records = Csv.decodeByName s 
  case records of (Left s) -> fail s
                  (Right (header, rows)) -> return $ toList rows



valuesOnly :: [DataPoint a b] -> [b]
valuesOnly = fmap dpV

toPair (DataPoint d v) = (d, v)

plot' :: PlotValue a => [DataPoint a Double] -> String -> FilePath -> IO ()
plot' ts title path = toFile def path $ do
  layout_title .= "XYZ"
  plot (line title [map toPair ts]) 



module Q.Time
        ( module Q.Time.Date
        , module Q.Time.DayCounter
        , parseLocalTime
        ) where

import Q.Time.Date
import Q.Time.DayCounter
import Data.Time
import Data.Time.Format
import Data.Time.Format.ISO8601

-- | Converts a shortened ISO08601 date string, or datetime to a LocalTime.
-- | If the string doesn't contain time then midnight is used.
parseLocalTime :: String -> Maybe LocalTime
parseLocalTime iso_datetime =
  if length iso_datetime == 8 then do
    day <- formatParseM dayFormat' iso_datetime
    return $ LocalTime day midnight
  else                
    formatParseM localTimeFormat' iso_datetime


-- | basic ISO08601 date/time format.
localTimeFormat' = localTimeFormat dayFormat' timeFormat'
-- | basic ISO08601 time format.
timeFormat' = timeOfDayFormat BasicFormat
-- | basic ISO08601 day format.
dayFormat' = calendarFormat BasicFormat

-- | Format a date as an basic ISO08601 format.
dateToString :: LocalTime -> String 
dateToString date = formatShow localTimeFormat' date

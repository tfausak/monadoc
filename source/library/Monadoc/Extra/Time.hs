module Monadoc.Extra.Time where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time

epoch :: Time.UTCTime
epoch = makeUtcTime 1970 1 1 0 0 0

makeUtcTime ::
  Time.Year ->
  Time.MonthOfYear ->
  Time.DayOfMonth ->
  Int ->
  Int ->
  Fixed.Pico ->
  Time.UTCTime
makeUtcTime year month day hour minute second =
  Time.UTCTime
    { Time.utctDay = Time.fromGregorian year month day,
      Time.utctDayTime =
        Time.timeOfDayToTime
          Time.TimeOfDay
            { Time.todHour = hour,
              Time.todMin = minute,
              Time.todSec = second
            }
    }

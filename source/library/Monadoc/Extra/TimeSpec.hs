module Monadoc.Extra.TimeSpec where

import qualified Data.Fixed as Fixed
import qualified Data.Time as Time
import qualified Monadoc.Extra.Time as Extra
import qualified Test.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "Monadoc.Extra.Time" $ do
  Hspec.describe "epoch" $ do
    Hspec.it "is correct" $ do
      Extra.epoch `Hspec.shouldBe` Extra.makeUtcTime 1970 1 1 0 0 0

  Hspec.describe "makeUtcTime" $ do
    Hspec.it "works" $ do
      let year = 2001 :: Time.Year
          month = 2 :: Time.MonthOfYear
          day = 3 :: Time.DayOfMonth
          hour = 4 :: Int
          minute = 5 :: Int
          second = 6.007 :: Fixed.Pico
          utcTime =
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
      Extra.makeUtcTime year month day hour minute second `Hspec.shouldBe` utcTime

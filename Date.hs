import Data.Time
import Data.Decimal

type Date = UTCTime

date :: String -> Date
date = parseTimeOrError True defaultTimeLocale dateFormat
  where dateFormat = "%Y-%m-%d-%H:%M-%Z"

t1 = date "2010-01-01-15:30-GMT"
t2 = date "2010-02-01-12:00-GMT"

-- instead of floats, we should be using Decimals integer
-- part means days decimal part means seconds
type Days = Double

-- mimicking the behaviour of double, but can be extended to
-- the proper (day,second) handling as well (subtracting the
-- floor of diff then add the remaining seconds...
diff :: Date -> Date -> Days
diff d1 d2 = realToFrac $ (diffUTCTime d1 d2 ) / secondsInDay
  where secondsInDay = (24 * 60 * 60)

add :: Date -> Days -> Date
add d1 days = addUTCTime seconds d1
  where
    seconds          = fromIntegral (daysInSeconds + remainingSeconds)
    secondsInDay     = (24 * 60 * 60)
    daysInSeconds    = (floor days) * secondsInDay
    remainingSeconds = truncate (fromIntegral secondsInDay * (days - fromIntegral (truncate days)))

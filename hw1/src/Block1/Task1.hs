module Block1.Task1
       ( Weekday(..),
       nextDay,
       afterDays,
       isWeekend,
       daysToParty
       ) where

data Weekday = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving(Show, Eq)

nextDay :: Weekday -> Weekday

nextDay Sun = Mon
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun

afterDays :: Weekday -> Int -> Weekday

afterDays day n 
    | n >= 7 || n <= -7 = afterDays day (mod n 7)
    | n < 0            = afterDays day (n + 7)
    | n > 0            = nextDay (afterDays day (n - 1))
    | otherwise        = day

isWeekend :: Weekday -> Bool

isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

daysToParty :: Weekday -> Int

daysToParty Fri = 0
daysToParty day = 1 + (daysToParty $ nextDay $ day)

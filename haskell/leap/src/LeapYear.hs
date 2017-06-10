module LeapYear (isLeapYear) where

-- Every year that is exactly divisible by four is a leap year, except
-- for years that are exactly divisible by 100, but these centurial
-- years are leap years if they are exactly divisible by 400.
-- https://en.wikipedia.org/wiki/Leap_year
isLeapYear year = (year `divisible` 400) ||
    ((year `divisible` 4) && not (year `divisible` 100))

divisible x y = (x `mod` y) == 0
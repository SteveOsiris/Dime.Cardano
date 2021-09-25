{-|
Module      : Time.Calendar.BaseCalendar
Description : This module defines the class constraint what will be applied on the clendar dataType
Copyright   : (c) Zakaria kadi
Maintainer  : zakaria.kadi123@outlook.com
Stability   : experimental
-}
module Time.Calendar.BaseCalendar
  ( 
    -- * Calendar class 
    Cal(..)
  ) where

import Time.Date
import Control.Exception.Assert 
 
-- | The Cal class defines the functions that every Calendar dataType must implement. 
class Cal a where 
    -- | This function indicates if the given date coincides with a holiday in a given calendar 
    isHoliday :: a -> Date -> Bool  
  
    -- | This function indicates for a given Calendar if a given date coincides with a business day
    isBusinessDay :: a -> Date -> Bool
    isBusinessDay calendar date
        | isWeekEnd date             = False
        | isHoliday calendar date    = False
        | otherwise                  = True

    -- | This function returns the next business day in a given Calendar 
    nextBusinessDay :: a -> Date -> Date
    nextBusinessDay calendar date
        | isBusinessDay calendar (succ date) = succ date
        | otherwise                          = nextBusinessDay calendar (succ date) 

    -- | This function returns the preceding business day in a given calendar              
    precedingBusinessDay :: a -> Date -> Date
    precedingBusinessDay  calendar date
        | isBusinessDay  calendar (pred date) = pred date
        | otherwise                            = precedingBusinessDay calendar (pred date)  

    -- | This function calculates the number of days between two dates in a given calendar 
    businessDaysBetween :: a -> Date -> Date -> Int
    businessDaysBetween calendar firstDate secondDate = 
        let 
          numberNonBusinessDate = length $ filter (\d -> not $ isBusinessDay calendar d)[firstDate .. secondDate]
          firstDateFromEnum   =  fromEnum secondDate
          secondDateFromEnumm = fromEnum firstDate
        in 
          assert (secondDateFromEnumm - firstDateFromEnum >=0) (secondDateFromEnumm - firstDateFromEnum - numberNonBusinessDate)  
       




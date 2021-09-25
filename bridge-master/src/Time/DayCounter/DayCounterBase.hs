{-|
Module      : Time.DayCounter.DayCounterBase 
Description : This module defines the base class of DayCounters 
Copyright   : (c) Zakaria kadi
Maintainer  : zakaria.kadi123@outlook.com
Stability   : experimental
-}
module Time.DayCounter.DayCounterBase 
   (
       -- * DayCounter Class  
       DayCounter(..)
   ) where 

import Time.Date 


-- |Base class of dayCounters
class DayCounter dc where 
    -- |Returns the name of the dayCounter 
    name         :: dc -> String 
   
    -- | Returns the numer of days between two dates 
    dayCount     :: dc -> Date -> Date -> Int 
   
    -- | Returns the period between two dates as a fraction of year  
    yearFraction :: dc -> Date -> Date -> Double   


{-|
Module      : Time.DayCounter.Actual365Fixed 
Description : This module defines the Actual 365 Fixed day counter. Source:https://en.wikipedia.org/wiki/Day_count_convention
Copyright   : (c) Zakaria kadi
Maintainer  : zakaria.kadi123@outlook.com
Stability   : experimental
-}
module Time.DayCounter.Actual365Fixed
   ( 
     -- *  Actual365Fixed Data Type     
   ) where 

import Time.DayCounter.DayCounterBase
import Time.Date

-- | The actual 365 Fixed dayCounter treats each month normally and the year is assumed to be 365 days 
--  For example, in a period from February 1, 2005 to April 1,2005, the factor is considered to be 59 days divided  by 365 
data Actual365Fixed = Actual365Fixed 


instance DayCounter Actual365Fixed where 
    name dc       = "Actual 365 Fixed" 
    
    dayCount dc   = daysBetween

    yearFraction dc d1 d2  = ( /(fromIntegral 365))$ fromIntegral $ dayCount dc d1 d2
 
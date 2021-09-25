{-|
Module      : Time.Calendar.FranceCalendar
Description : This module defines the FranceCalendar DataType. It can be used as an example for other calendars 
Copyright   : (c) Zakaria kadi
Maintainer  : zakaria.kadi123@outlook.com
Stability   : experimental
-}
module Time.Calendar.FranceCalendar
  ( 
   -- * French calendar constructor: It doesn't take any argument
   FranceCalendar(..)
  ) where 
  
import Time.Date
import Time.Calendar.BaseCalendar
import Control.Exception.Assert  


-- | Simple definition of the FrancceCalendar dataType
data FranceCalendar = FranceCalendar 


-- | FranceCalendar is an instance of the Cal typeClass 

instance Cal FranceCalendar where 
    -- | This function indicates if the given date coincides with a holiday in a the french Calendar 
    isHoliday  _ d = isFranceFixedDateHoliday d || isFranceFloatingDateHoliday d
    



-- | Indicates whether a date belongs to fixed date holiday 
isFranceFixedDateHoliday :: Date -> Bool
isFranceFixedDateHoliday d
    = (getDay d,getMonth d) `elem` [(1,1),(1,5),(8,5),(14,7),(15,8),(1,11),(11,11),(25,12)]    
    
-- | Indicates whether a date belongs to floating date holiday 
isFranceFloatingDateHoliday :: Date -> Bool
isFranceFloatingDateHoliday d
     -- A valid date has a year greater or equal to 1900
    |   n == easterMonday            = True             --  Lundi de paques
    |   n == easterMonday + 38       = True             --  Jeudi de l'ascension
    |   n == easterMonday + 49       = True             --  Lundi de Pentecôte
    |   otherwise                    = False 

  where 
    n                 = (fromEnum d)- (fromEnum $ mkDate (year-1) 12 31)
    year              = getYear d
    easterMonday      = easterMondayDates!!(year-1900)
    easterMondayDates =  
                 [  106,98,  90, 103,  95, 114, 106,  91, 111, 102,        
                    87, 107,  99,  83, 103,  95, 115,  99,  91, 111, 
                    96,  87, 107,  92, 112, 103,  95, 108, 100,  91,
                    111,  96,  88, 107,  92, 112, 104,  88, 108, 100,
                    85, 104,  96, 116, 101,  92, 112,  97,  89, 108, 
                    100,  85, 105,  96, 109, 101,  93, 112,  97,  89,  
                    109,  93, 113, 105,  90, 109, 101,  86, 106,  97,   
                    89, 102,  94, 113, 105,  90, 110, 101,  86, 106,  
                    98, 110, 102,  94, 114,  98,  90, 110,  95,  86,   
                    106,  91, 111, 102,  94, 107,  99,  90, 103,  95,   
                    115, 106,  91, 111, 103,  87, 107,  99,  84, 103,  
                    95, 115, 100,  91, 111,  96,  88, 107,  92, 112,   
                    104,  95, 108, 100,  92, 111,  96,  88, 108,  92,   
                    112, 104,  89, 108, 100,  85, 105,  96, 116, 101,   
                    93, 112,  97,  89, 109, 100,  85, 105,  97, 109,   
                    101,  93, 113,  97,  89, 109,  94, 113, 105,  90,   
                    110, 101,  86, 106,  98,  89, 102,  94, 114, 105,  
                    90, 110, 102,  86, 106,  98, 111, 102,  94, 114,   
                    99,  90, 110,  95,  87, 106,  91, 111, 103,  94,   
                    107,  99,  91, 103,  95, 115, 107,  91, 111, 103,   
                    88, 108, 100,  85, 105,  96, 109, 101,  93, 112,   
                    97,  89, 109,  93, 113, 105,  90, 109, 101,  86,   
                    106,  97,  89, 102,  94, 113, 105,  90, 110, 101,   
                    86, 106,  98, 110, 102,  94, 114,  98,  90, 110,  
                    95,  86, 106,  91, 111, 102,  94, 107,  99,  90,  
                    103,  95, 115, 106,  91, 111, 103,  87, 107,  99,  
                    84, 103,  95, 115, 100,  91, 111,  96,  88, 107,   
                    92, 112, 104,  95, 108, 100,  92, 111,  96,  88,  
                    108,  92, 112, 104,  89, 108, 100,  85, 105,  96,   
                    116, 101,  93, 112,  97,  89, 109, 100,  85, 105    
                 ] 


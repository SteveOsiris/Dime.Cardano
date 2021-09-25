
module Time.Date 
  ( 
    -- * Date type 
    Date
    -- * Date constructor
    , mkDate
    -- * Date getter 
    , getDay
    , getMonth
    , getYear
    -- * Date functions 
    , addDays 
    , addMonths 
    , addYears   
    -- * Some useful functions     
    , daysOfMonth
    , dayOfWeek
    , isWeekEnd 
    , isEndOfMonth
    , isFirstOfMonth
    , isLeapYear
    , daysBetween
  ) where

import Control.Exception.Assert 
 
-- | Date DataType
data Date = Date 
    { year   :: !Int -- ^ Year
    , month  :: !Int -- ^ Month
    , day    :: !Int -- ^ Day
    } deriving(Eq,Ord)   

-- |Constructor of valid dates 
mkDate :: Int -> Int -> Int -> Date    

mkDate  y m d = assert (isValidDate $ Date y m d) (Date y m d)

-- | Day getter 
getDay = day 

-- | Month getter 
getMonth = month

-- | Year getter
getYear = year
   
-- | Checks is a given date is valid
-- A Day is valid only if : Year is between 1900 and 2199 
-- Month is between  1 and 12 
-- Day is between 1 and the number of days of the corresponding month 
isValidDate :: Date -> Bool
isValidDate (Date y m d) 
       | (y < 1900) || (y>  2199) || (m>12) || (m<=0) || (d <= 0) = False 
       |  d > (daysOfMonth y m)                                   = False
       | otherwise                                                = True
  
-- | Number of days in a given month 
daysOfMonth :: Int -> Int -> Int 
daysOfMonth year month 
    | month == 2 = 28 + if (isLeapYear year) then 1 else 0
    | otherwise  = 31 -  rem (rem (month-1) 7) 2

-- | Indicates if a given year is a Leap year or not 
isLeapYear :: Int -> Bool  
isLeapYear year 
    | ((rem year 400) == 0) || (((rem year 100)/=0 ) && ((rem year 4) == 0 )) = True 
    |  otherwise = False   
    
-- for a given date we need to know  
--          the corresponding day of the week. 
--          is it a week end or not. 
--           
-- | Days of the week 
data DayOfWeek = Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday 
           deriving (Eq,Show,Ord,Enum,Read)
-- | Returns the day of the week of a given date  using Tomohiko Sakamoto’s Algorithm
dayOfWeek :: Date -> DayOfWeek
dayOfWeek  (Date year month day) =
       let  
            t = [0,3,2, 5, 0, 3, 5, 1, 4, 6, 2, 4 ]
            y = (if (month <3) then year-1 else year)
       in
       toDayOfWeek(rem (y +  (div y 4) - (div y 100) + (div y 400) + t!!(month - 1) + day)  7)

-- | Converts the results of Tomohiko Sakamoto’s Algorithm to DayOfWeek Type        
toDayOfWeek :: Int -> DayOfWeek 
toDayOfWeek x 
   | x==0 = Sunday 
   | otherwise = toEnum (x-1)::DayOfWeek  
   
-- |  Returns true is the given date is a weekend 
isWeekEnd :: Date -> Bool  
isWeekEnd date 
    | (dayOfWeek date == Sunday) || (dayOfWeek date == Saturday) = True
    | otherwise                                                  = False

-- | Date Show instance 
instance Show Date where 
  show date = 
       let 
           Date y m d = date 
       in 
           show y ++ "/" ++ (if (m<10) then "0" else "" )++ show m ++"/" ++ (if (d<10) then "0" else "" ) ++ show d

--  | Date Enum instance 
instance Enum Date where   

   succ (Date y 12 31) = mkDate (y+1) 1 1
   succ (Date y m d) 
      | isEndOfMonth (Date y m d) =  mkDate y (m+1) 1 
      | otherwise                 =  mkDate y m (d+1) 
 
   pred (Date y 1 1)  =mkDate (y-1) 12 31
   pred (Date y m d) 
      | isFirstOfMonth (Date y m d) = mkDate y (m-1) (daysOfMonth y m)
      | otherwise                   = mkDate y m (d-1) 
    
   toEnum     n  
      | n<=1     = Date 1900 1 1  -- it's a default date 
      | otherwise = 
            let 
              years =  takeWhile (<n) $ scanl (\acc y -> acc + if isLeapYear y then 366 else 365) 0 [1900..]
              year = (length years) + 1899 
              months =  takeWhile (< n -(last years) ) $ scanl (\acc m -> acc + (daysOfMonth year m)) 0 [1..12] 
              month  =  length months  
              day =  n-(last years)-(last months) 
             in (mkDate year month day)  
   

   fromEnum (Date 1900 1 1) = 1 
   fromEnum (Date y 1 1)  = 1 + (sum $  map ( \x -> if (isLeapYear x) then 366 else 365) [1900 .. (y-1)] )
   fromEnum (Date y m 1)  =  (sum $  map ( \m -> daysOfMonth y m) [1..(m-1)] ) + fromEnum (Date y 1 1)
   fromEnum (Date y m d)  = d - 1 + fromEnum (Date y m 1)               
 

-- | Indicates if a day is the end of month 
isEndOfMonth :: Date -> Bool
isEndOfMonth (Date y m d) 
      | (daysOfMonth y m)==d = True
      | otherwise            = False  

-- | Indicates if a date corresponds to the first date of the month 
isFirstOfMonth :: Date -> Bool 
isFirstOfMonth (Date y m d)  
   | d==1 = True
   | otherwise = False
   
-- | 12 months names 
months :: [String] 
months = ["january",
          "february",
          "march",
          "april",
          "may",
          "june",
          "july",
          "august",
          "september",
          "october",
          "november",
          "december"]
  

-- | Adds n days to a given date : n can be negatif
addDays :: Date -> Int -> Date  
addDays (Date y m d) n = (toEnum $ (fromEnum $ (Date y m d)) + n ) :: Date

-- | This function Adds a given number of months (n) to a given date : n can be negatif 
addMonths :: Date ->  Int -> Date 
addMonths date n 
    | n==0         = mkDate y m d
    | n>0          = foldl (\acc x -> addMonth acc) date [1 .. n]
    | otherwise    = foldl (\acc x -> substructMonth acc) date [1 .. (-n)]
  where 
    Date y m d = date  

-- | This function adds one month to a given date
addMonth :: Date -> Date 
addMonth (Date y m d)
    | m==12        = mkDate (y+1) 1 d 
    | otherwise    = mkDate y (m+1) d

-- | This function substructs one month from a given date
substructMonth (Date y m d)
    | m==1        = mkDate (y-1) 12 d 
    | otherwise    = mkDate y (m-1) d           
    
-- | This function adds a given number of years (n) to a given date : n can be negatif 
addYears :: Date -> Int -> Date 
addYears (Date y m d) n = mkDate (y+n) m n

-- | Calculates the number of days between two date 
daysBetween :: Date -> Date -> Int 
daysBetween d1 d2  = let diff =(fromEnum d2) - (fromEnum d1) in assert (diff >=0) diff


module App.Text where

similar word compareWord | word == compareWord = False
similar word compareWord =
    variesBySize word compareWord || variesByChars word compareWord

variesByChars w1 w2 =
    characterDifference w1 w2 == 1 && lengthDifference w1 w2 == 0

variesBySize w1 w2 =
    lengthDifference w1 w2 == 1 && characterDifference w1 w2 == 0
    
characterDifference word compareWord =
    length $ filter (==False) $ zipWith (==) word compareWord

lengthDifference word compareWord =
    abs $ length word - length compareWord
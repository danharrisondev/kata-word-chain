module App.Text where

similar word compareWord | word == compareWord = False
similar word compareWord =
    variesByChars word compareWord

variesByChars w1 w2 = characterDifference w1 w2 == 1
    
characterDifference word compareWord =
    length $ filter (==False) $ zipWith (==) word compareWord
sumsg :: Integer -> Integer
-- Insert your own code here.


-- Do not change the following wrapper code
wrapper :: String -> Integer
wrapper input = sumsg (read input::Integer)

main =  print . wrapper =<< getLine

selfrle :: [Int]
-- Insert your own code here.





-- Do not change the fololowing wrapper code
wrapper :: String -> [Int]
wrapper input = take (read input::Int) selfrle

main =  print . wrapper =<< getLine

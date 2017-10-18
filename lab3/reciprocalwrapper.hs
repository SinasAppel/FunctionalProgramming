longestRepetition :: Int -> Int -> Int

-- Insert your own code here.




-- Do not change the following wrapper code
wrapper :: [String] -> Int
wrapper (a:b:_) = longestRepetition (read a::Int) (read b::Int)

main =  print . wrapper . words =<< getLine

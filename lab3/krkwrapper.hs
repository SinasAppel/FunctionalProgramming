krk :: String -> String -> String -> Int
-- Insert your own code here.

-- Do not change the following wrapper code
  
wrapper :: String -> Int
wrapper input = krk whiteKing whiteRook blackKing
  where whiteKing = take 2 input
        whiteRook = take 2 (drop 3 input)
        blackKing = take 2 (drop 6 input)
        
main =  print . wrapper =<< getLine

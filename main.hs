
-- Add random function where is number 2 to generate sequence
main = do 
            verifyPositions 2

verifyPositions sequenceRandom = do 
                                      putStr "What's the sequence? " 
                                      sequenceUser <- getLine
                                      if(read sequenceUser) == sequenceRandom
                                          then do 
                                                putStrLn "You WIN!"
                                          else do
                                               putStrLn "Not this time :("
                                               verifyPositions sequenceRandom
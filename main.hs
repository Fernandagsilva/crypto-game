--Nome: Fernanda Gonçalves da Silva
--Matrícula: 201765102C

import Commons

main = do sequenceRandom <- generateSequence 4
          startGame sequenceRandom 0

startGame sequenceRandom count = 
         do putStr "What's the sequence? " 
            userEntry <- getLine
            let sequenceUserEntry = convert userEntry
            let completedCount = completedPositions sequenceUserEntry sequenceRandom
            let newSequenceRandom = removeCompletedPositions sequenceUserEntry sequenceRandom
            let newSequenceUser = removeCompletedPositions sequenceRandom sequenceUserEntry
            let partialPositions = partialCount newSequenceUser newSequenceRandom
            let totalPositions = 4
            let rounds = count + 1
            print sequenceRandom
            if completedCount == totalPositions
            then do 
                  putStr "Completed: "
                  print completedCount
                  putStr "Partial: "
                  print partialPositions
                  putStr "Congratulations! You won after "
                  putStr (show rounds)
                  putStrLn " rounds."
            else do
                  putStr "Completed: "
                  print completedCount
                  putStr "Partial: "
                  print partialPositions
                  startGame sequenceRandom rounds
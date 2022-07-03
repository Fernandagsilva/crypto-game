--Nome: Fernanda Gonçalves da Silva
--Matrícula: 201765102C

module Commons where
import System.Random

-- generate a list of int
generateSequence :: Int -> IO ([Int])
generateSequence 0 = return []
generateSequence n = do
  r  <- randomRIO (1::Int, 6) 
  rs <- generateSequence (n-1)
  return (r:rs) 


-- Converts an user entry to an integer list
convert :: Read a => String -> [a]
convert = map read . words


-- Verify the count of completed positions between two lists
completedPositions :: [Int] -> [Int] -> Int
completedPositions [] [] = 0 
completedPositions (x:xs) (y:ys)
                | (x == y) = (completedPositions xs ys)+1
                | otherwise = (completedPositions xs ys)


-- Removes all completed positions and return a list with the others elements
removeCompletedPositions :: [Int] -> [Int] -> [Int]
removeCompletedPositions [] [] = [] 
removeCompletedPositions (x:xs) (y:ys) =
        if (x == y) 
        then do
                removeCompletedPositions xs ys
         else do
                [x]++(removeCompletedPositions xs ys)


-- Removes a partial element from the list
removePartialElement :: Int -> [Int] -> [Int]
removePartialElement element [] = [] 
removePartialElement element (x:xs) =
                if (x == element) 
                then do
                    removePartialElement element xs
                else do
                    [x]++(removePartialElement element xs)


-- Verify if an element exist in the list
verifyPartialPosition :: Int -> [Int] -> Bool
verifyPartialPosition _ [] = False 
verifyPartialPosition element (x:xs)
                | (x == element) = True
                | otherwise = (verifyPartialPosition element xs)


-- Count the number of partial elements in the list
partialCount :: [Int] -> [Int] -> Int
partialCount [] _ = 0 
partialCount (x:xs) sequence =
    do
        let existPartialPosition = verifyPartialPosition x sequence 
        if (existPartialPosition == True)
        then do
            let newSequence = removePartialElement x sequence
            (partialCount xs newSequence)+1
        else do 
            partialCount xs sequence
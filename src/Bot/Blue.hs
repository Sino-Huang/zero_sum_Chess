-- Assignment completed by
-- Name    : SUKAI HUANG
-- UID     : u6492211
-- Tutor   : Debashish Chakraborty
-- Lab Time: Thursday 3:00 pm
module Bot.Blue where

import Data.Board
import Data.Player


-- doctest
-- Only makeMove can be tested since other functions are the sub functions in makeMove
-- | makeMove
-- >>> makeMove testBoard112 3
-- 5
--
-- block test, the makeMove will block the opponent to prevent lose
-- >>> makeMove testBoard11 3
-- 5
--
-- test the lookAhead
-- >>> makeMove testBoard4 1
-- 4
--
-- >>> makeMove testBoard4 3
-- 3
--



makeMove :: Board -> LookAhead -> Int
makeMove initboard lookahead= decision initboard
    where
        -- adjust indexlist
        width :: Int
        width = fst $ dimension initboard

        indexList :: [Int]
        indexList = [(width `div` 2 +1) .. width] ++ [1.. (width `div` 2)] -- modified indexList so that the move will be focused on the middle of the board

        --alphabetapruning function -- output the heuristic value from the bottom board with pruning
        alphabetapruning :: Board -> Int -> Int -> Int -> Bool -> Int
        alphabetapruning bo depth alpha beta whethermax     -- whethermax is a bool input to signify the minimax state
            | bo == initboard = (-2000) -- avoid take the invalid move
            | depth == 0 || turn bo == Finished = heuristicValue bo -- check whether reaching the leaves
            | otherwise = case whethermax of
                True  -> maxfoldl max (-1000) indexList

                False -> minfoldl min (1000) indexList
            where
                -- two modified foldl functions are created for the purpose of alpha beta pruning
                maxfoldl :: (Int -> Int -> Int ) -> Int -> [Int] -> Int
                maxfoldl _ preval [] = preval
                maxfoldl f preval (x:xs)
                              | preval >= beta = preval -- pruning happens when alpha >= preval
                              | otherwise = maxfoldl f (f preval
                              -- in maximising part, if previous value is bigger than the current alpha, it will replace the current alpha
                                                               ( alphabetapruning (updateBoard bo x) (depth - 1) (max alpha preval) beta False ))
                                                                xs

                minfoldl :: (Int -> Int -> Int ) -> Int -> [Int] -> Int
                minfoldl _ preval [] = preval
                minfoldl f preval (x:xs)
                               | preval <= alpha = preval
                               | otherwise = minfoldl f
                                                            (f preval
                               -- in minimising part, if previous value is smaller than the current beta, it will replace the current beta
                                                               ( alphabetapruning (updateBoard bo x) (depth - 1) alpha (min preval beta) True ))
                                                                 xs

        --Actions (state) -- maximizing Min-Value(Result(a,state))
        -- zip the heuristic value and the index together and then output the index with the largest heuristic value.
        decision :: Board -> Int
        decision board1 = choose $ zip (map' (\y x-> alphabetapruning x lookahead y 1000 False ) (-1000)(map (\x -> updateBoard board1 x) indexList )) indexList
            where
                -- map' function to transfer the alpha beta function to the next node
                map' :: (Int -> Board -> Int ) -> Int -> [Board] -> [Int]
                map' f _ [] = []
                map' f num (x:xs) = f num x : map' f (f num x) xs
                -- choose function output the index number from the (minimax value, index) tuple and thus tell the makeMove function the best move
                choose :: [(Int,Int)] -> Int
                choose (x:xs:xss)
                    | (fst x) >= (fst xs) = choose (x:xss)
                    | otherwise = choose (xs:xss)
                choose (x:[]) = snd x
                choose [] = error"something wrong"

        -- heuristicValue function , since it is zero-sum game, the heuristicValue will be the score of the player minus the score of the opponent
        heuristicValue :: Board -> Int
        heuristicValue b
            | turn initboard == BlueBot = blueScore b - redScore b
            | otherwise  = redScore b - blueScore b


-- here are some functions and board for the purpose of doctest
initBoard :: Board
initBoard = initialiseBoard (7,6) BlueBot 4
anchorBoard :: Board -> LookAhead -> Board
anchorBoard board1 look
    | look > 1 = case board1 of
        Board _ _ _ BlueBot _ _-> anchorBoard (updateBoard board1 myIndex) (look - 1)
        Board _ _ _ RedBot _ _-> anchorBoard (updateBoard board1 yourIndex) (look - 1)
        _ -> error"it ends"
    | look == 1 = case board1 of
        Board _ _ _ BlueBot _ _-> updateBoard board1 myIndex
        Board _ _ _ RedBot _ _-> updateBoard board1 yourIndex
        _ -> error"it ends"
    | otherwise = error "LookAhead must be positive integer."
    where
        width = fst $ dimension board1
        myIndex = width `div` 2
        yourIndex = width `div` 2 + 1
testBoard1 :: Board
testBoard1 = anchorBoard initBoard 4
testBoard2 :: Board
testBoard2 = anchorBoard initBoard 6
testBoard3 :: Board
testBoard3 = anchorBoard initBoard 7
testBoard4 :: Board
testBoard4 = anchorBoard initBoard 5
testBoard5 :: Board
testBoard5 = updateBoard testBoard4 3
testBoard6 :: Board
testBoard6 = updateBoard testBoard5 2
testBoard7 :: Board
testBoard7 = updateBoard testBoard6 4
testBoard8 :: Board
testBoard8 = updateBoard testBoard7 4
testBoard9 :: Board
testBoard9 = updateBoard testBoard8 6
testBoard10 :: Board
testBoard10 = updateBoard testBoard9 5
testBoard12 :: Board
testBoard12 = updateBoard testBoard10 7
testBoard13 :: Board
testBoard13 = updateBoard testBoard12 2
testBoard11 :: Board
testBoard11 = updateBoard testBoard10 1
testBoard112 :: Board
testBoard112 = updateBoard testBoard11 2
testBoard113 :: Board
testBoard113 = updateBoard testBoard112 5
testBoard114 :: Board
testBoard114 = updateBoard testBoard113 1
module Bot.Red where
import Data.Board
import Data.Player

--Alpha-Beta-Decision(state)
makeMove :: Board -> LookAhead -> Int
makeMove initboard lookahead= decision initboard
    where
        -- better indexlist
        width :: Int
        width = fst $ dimension initboard

        indexList :: [Int]
        indexList = [(width `div` 2 +1) .. width] ++ [1.. (width `div` 2)]

        --alphabetapruning function -- output the heuristic value
        alphabetapruning :: Board -> Int -> Int -> Int -> Bool -> Int
        alphabetapruning bo depth alpha beta whethermax
            | depth == 0 || turn bo == Finished = heuristicValue bo
            | otherwise = case whethermax of
                True  -> maxfoldl max (-1000) indexList

                False -> minfoldl min (1000) indexList
            where
                maxfoldl :: (Int -> Int -> Int ) -> Int -> [Int] -> Int
                maxfoldl _ preval [] = preval
                maxfoldl f preval (x:xs)
                              | preval >= beta = preval
                              | otherwise = maxfoldl f (f preval
                                                               ( alphabetapruning (updateBoard bo x) (depth - 1) (max alpha preval) beta False ))
                                                                xs

                minfoldl :: (Int -> Int -> Int ) -> Int -> [Int] -> Int
                minfoldl _ preval [] = preval
                minfoldl f preval (x:xs)
                               | preval <= alpha = preval
                               | otherwise = minfoldl f
                                                            (f preval
                                                               ( alphabetapruning (updateBoard bo x) (depth - 1) alpha (min preval beta) True ))
                                                                 xs

        --Actions (state) -- maximizing Min-Value(Result(a,state))
        decision :: Board -> Int
        decision board1 = choose $ zip (map (\x -> alphabetapruning x lookahead (-1000) 1000 False ) (map (\x -> updateBoard board1 x) indexList )) indexList
            where

                choose :: [(Int,Int)] -> Int
                choose (x:[]) = snd x
                choose (x:xs:xss)
                    | (fst x) >= (fst xs) && (updateBoard board1 (snd x) /= board1)= choose (x:xss)
                    | otherwise = choose (xs:xss)

                choose [] = error"something wrong"

        heuristicValue :: Board -> Int -- since it is zero-sum game, the heuristicValue will be the sum of the value
        heuristicValue b
            | turn initboard == BlueBot = blueScore b - redScore b
            | otherwise  = redScore b - blueScore b











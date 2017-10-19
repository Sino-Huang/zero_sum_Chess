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
        indexList = [4,3..1] ++ [5..width]


        --Max-Value function
        maxValue :: Board -> Int -> Int -> Int -> Int
        maxValue board1 alpha beta depth
            | turn board1 == Finished = heuristicValue board1
            | depth == 0 = heuristicValue board1
            | otherwise = maxfoldl (\x1 x2 -> max x1 x2) (-1000) indexList
                where
                    maxfoldl ::  (Int -> Int -> Int) -> Int -> [Int] -> Int
                    maxfoldl _ x [] = x
                    maxfoldl f x [m] = (f x (minValue (updateBoard board1 m) alpha beta (depth -1)))
                    maxfoldl f x (y:ys:yss)
                        | f x (minValue (updateBoard board1 y) alpha beta (depth -1)) >= beta = f x (minValue (updateBoard board1 y) alpha beta (depth -1))
                        | otherwise = if
                                        (f x (minValue (updateBoard board1 y) alpha beta (depth -1))) > alpha
                                        then maxfoldl f (f (f x (minValue (updateBoard board1 y) alpha beta (depth -1))) (minValue (updateBoard board1 ys) (f x (minValue (updateBoard board1 y) alpha beta (depth -1))) beta  (depth -1))) yss
                                        else maxfoldl f (f (f x (minValue (updateBoard board1 y) alpha beta (depth -1))) (minValue (updateBoard board1 ys) alpha beta (depth -1))) yss



        --Min-Value function
        minValue :: Board -> Int -> Int -> Int -> Int
        minValue board2 alpha beta depth
            | turn board2 == Finished = heuristicValue board2
            | depth == 0 = heuristicValue board2
            | otherwise  = minfoldl  (\m1 m2 -> min m1 m2) (1000) indexList
                where
                    minfoldl ::  (Int -> Int -> Int) -> Int -> [Int] -> Int
                    minfoldl _ x [] = x
                    minfoldl f x [m] = (f x (maxValue (updateBoard board2 m) alpha beta (depth -1)))
                    minfoldl f x (y:ys:yss)
                        | f x (maxValue (updateBoard board2 y) alpha beta (depth-1) ) <= alpha = f x (maxValue (updateBoard board2 y) alpha beta (depth -1))
                        | otherwise = if
                                        (f x (maxValue (updateBoard board2 y) alpha beta (depth -1))) < beta
                                        then minfoldl f (f (f x (maxValue (updateBoard board2 y) alpha beta (depth -1))) (maxValue (updateBoard board2 ys) alpha (f x (maxValue (updateBoard board2 y) alpha beta (depth -1)))(depth -1) )) yss
                                        else minfoldl f (f (f x (maxValue (updateBoard board2 y) alpha beta (depth -1))) (maxValue (updateBoard board2 ys) alpha beta (depth -1))) yss


        --Actions (state) -- maximizing Min-Value(Result(a,state))
        decision :: Board -> Int
        decision board1 = choose $ zip (map (\x -> minValue x (-1000) 1000 (lookahead -1) ) (successor board1)) indexList
            where
                choose :: [(Int,Int)] -> Int
                choose [] = error"something wrong"
                choose (x:[]) = snd x
                choose (x:xs:xss)
                    | (fst x) >= (fst xs) && (validMove board1 (snd x))= choose (x:xss)
                    | otherwise = choose (xs:xss)


        --successors function

        successor :: Board -> [Board]
        successor board1 = map' (\m -> updateBoard board1 m) indexList
            where
                map':: (Int -> b) -> [Int] -> [b]
                map' _ [] = []
                map' f (x:xs)
                    | turn board1 == Finished = []
                    | otherwise = f x : map' f xs



        heuristicValue :: Board -> Int -- since it is zero-sum game, the heuristicValue will be the sum of the value
        heuristicValue b = redScore b - blueScore b








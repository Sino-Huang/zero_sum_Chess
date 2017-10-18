module Bot.Red where


import Data.Board
import Data.Player

type ABValue = (Max,Min) -- this is the ab pruning tuple

type Max = Int
type Min = Int
type MiniMax = Int -- this is the minimax value for that board state

makeMove :: Board -> LookAhead -> Int
makeMove currentBoard look = decision  (zip (map' (\x -> miniMax x (look-1)) (map (\y -> updateBoard currentBoard y) indexList)) (indexList))


    where
        map' :: (a->b) -> [a] -> [b]
        map' _ [] = []
        map' f (x:xs)
            | True = f x : map f xs
            | otherwise = []
        -- decision function
        decision :: (Ord a ) => [(a,Int)] -> Int
        decision [] = 1
        decision (x:[]) = snd x
        decision (x:xs:xss)
            | fst x >= fst xs && (validMove currentBoard (snd x))= decision (x:xss)
            | otherwise = decision (xs:xss)
        --
        -- better indexlist
        width = fst $ dimension currentBoard
        firstList = reverse [1 .. (width `div` 2)]
        secondList = [(width `div` 2 + 1 ) .. width ]
        combine :: [Int] -> [Int] -> [Int]
        combine lst1 [] = lst1
        combine [] lst2 = lst2
        combine (x:xs) (y:ys) = x:y:combine xs ys
        indexList =  combine firstList secondList






        miniMax :: Board -> LookAhead -> MiniMax
        miniMax board1 lk
            | lk == 0 = case board1 of
                Board _ _ _ BlueBot _ _-> foldl' (\x y -> min x $ heuristicValue (updateBoard board1 y)) (1000) indexList
                Board _ _ _ RedBot _ _ -> foldl' (\x y -> max x $ heuristicValue (updateBoard board1 y)) (-1000) indexList
                Board _ _ _ Finished _ _ -> heuristicValue board1
            | lk > 0 = case board1 of
                    Board _ _ _ BlueBot _ _-> foldl' (\m n -> min m $ miniMax n (lk-1)) (1000) (map (\y -> updateBoard board1 y) indexList)
                    Board _ _ _ RedBot _ _ -> foldl' (\m n -> max m $ miniMax n (lk-1)) (-1000) (map (\y -> updateBoard board1 y) indexList)
                    Board _ _ _ Finished _ _ -> heuristicValue board1
            | otherwise = error"lk bigger than 0"

            where
                -- foldl special
                foldl' :: (a->b->a) -> a -> [b] -> a
                foldl' _ x [] = x
                foldl' f x (y:ys) = foldl' f (f x y) ys




        heuristicValue :: Board -> MiniMax -- since it is zero-sum game, the heuristicValue will be the sum of the value
        heuristicValue b =  redScore b - blueScore b




-- Nim

import Data.Char -- for digitToInt
import System.IO -- for hFlush

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row-1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
  where update r n = if r == row then n-num else n
  
putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))
                    
putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

newline :: IO ()
newline = putChar '\n'

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     --x:xs <- getLine
                     --newline -- putChar '\n'
                     if isDigit x then
                       --do hFlush stdin
                          return (digitToInt x)
                     else
                       do putStrLn "ERROR: Invalid digit"
                          --hFlush stdin
                          getDigit prompt

play :: Board -> Int -> IO ()
play board player =
  do --newline
     putBoard board
     if finished board then
       do newline
          putStr "Player "
          putStr (show (next player))
          putStrLn " wins!!"
     else
       do newline
          putStr "Player "
          putStr (show player)
          row <- getDigit "Enter a row number : "
          num <- getDigit "Enter a number of stars to remove in the row: "
          if valid board row num then
            play (move board row num) (next player)
          else
            do newline
               putStrLn "ERROR : Invalid move!!!!!"
               play board player

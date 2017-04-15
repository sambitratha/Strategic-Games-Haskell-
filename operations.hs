module Operations where

import Data.List
import System.Random
import Graphics.UI.GLUT

create_board_state :: Int -> [Char] -- creates the initial board_state list
create_board_state n = ['1' | k <- [1..n']]
	where n' = fromIntegral n

de_normalize :: ( GLfloat, GLfloat) -> (Integer, Integer) -- converts the normalized coordinates to row number, col number
de_normalize (x,y) = ( 1+floor (((1-y)/0.1)) ,1+floor (((x + 1) /0.1)) )

getindex :: [Int] -> Int -> Int 
getindex (x:xs) 0 = x
getindex (x:xs) index = getindex xs (index-1)

initBoard :: StdGen -> Int -> [Char]
initBoard seed len = getlist (len-1) (randomlist len seed) ""


randomlist :: Int -> StdGen -> [Int]
randomlist n = take n . unfoldr (Just . random)

--takes an integer array and returns a character array according to the entries of integer array
getlist ::Int -> [Int] ->[Char]-> [Char]
getlist (-1) list board = board
getlist index list board=
	if (getindex list index) `mod` 10 == 0
		then
			getlist (index -1) list (board ++ "0")
		else
			getlist (index -1) list (board ++ "1")



points :: Int  -> Int -> [(GLfloat, GLfloat, GLfloat)] -- generates mxn coordinates of the centre of squares that will fill the window and returns that list
points n 1 = [ (-0.95, (0.95-((fromIntegral (k-1))*0.1)) , 0) | k <- [1..n'] ]
	where n' = fromIntegral n
points n m = (points n (m-1)) ++ [ ((-0.95 + (fromIntegral (m-1))*0.1), (0.95-(fromIntegral (k-1))*0.1), 0) | k <- [1..n'] ] 
	where n' = fromIntegral n

normalize_plz :: (GLint, GLint) -> (GLfloat, GLfloat) -- normalizes the pixel coordinate to the range (-1 to 1, -1 to 1)
normalize_plz (x,y) = do
	let width = 600
	let height = 600
	((fromIntegral (toInteger x)-(width/2))/(width/2), (-(fromIntegral (toInteger y)-(height/2))/(height/2)))

getLength :: [Integer] -> Integer
getLength [] = 0
getLength (x:xs) = 1 + getLength xs

--appends to integer arrays
 
append :: [Integer] -> Integer -> [Integer]
append [] val = [val]
append (x:xs) val = x:(append xs val)

getCharacter :: [Char] -> Integer -> Char
getCharacter (x:xs) 0 = x 
getCharacter (x:xs) index = 

	if index > 0
		then

			getCharacter xs (index - 1)
		else
			getCharacter (x:xs) 0

getInt::[Integer] -> Integer -> Integer
getInt [] index = 0
getInt (x:xs) 0 = x 
getInt (x:xs) index = 
	if index > 0
		then
			getInt xs (index - 1)
		else
			getInt (x:xs) 0

--sets an index of a character array to a given input

setElement::[Char] -> Integer -> Char -> [Char]
setElement (x:xs) 0 val = val:xs
setElement (x:xs) index val = x:(setElement xs (index - 1) val)

getZero :: Integer
getZero = 0
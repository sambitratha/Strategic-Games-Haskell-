module GameHandle where

import Data.IORef
import Control.Monad
import Search
import Operations
import Graphics.UI.GLUT	

checkifwon :: [Char] -> [Char] ->Integer -> Bool
checkifwon board boardstate index =
	if index == 0 
		then
			if getCharacter board index == '0'
				then 
					if getCharacter boardstate index == '2'
						then True
					else
						False
				else
					if getCharacter boardstate index == '1'
						then 
							False
						else
							True
		else
			if getCharacter board index == '0'
				then
					if getCharacter boardstate index == '2'
						then 
							checkifwon board boardstate (index -1)

						else
							False
				else
					if getCharacter boardstate index == '0'
						then 
							checkifwon board boardstate (index -1)
						else
							False

changeboardstate :: [Char] -> [Char] -> Integer -> [Char]
changeboardstate board boardstate index = 
	if index == 0
		then
			if getCharacter board index == '0'
				then
					setElement boardstate index '0'

				else
					boardstate

		else
			if getCharacter board index == '0'
				then 
					changeboardstate board (setElement boardstate index '0') (index -1)

				else
					changeboardstate board boardstate (index-1)

getTotalMines::[Char] -> Integer -> Integer->Integer
getTotalMines board 0 rs = rs
getTotalMines board index rs =
	if getCharacter board (index -1) == '0'
		then
			getTotalMines board (index -1) (rs +1)
		else
			getTotalMines board (index-1) rs

modify_boardstate :: Bool -> Bool -> IORef [Char] -> IORef [Char] -> Integer -> IO () -- modifies the boardstate list
modify_boardstate left_click flag board1 _boardstate i = do
	board <- readIORef board1
	boardstate <- readIORef _boardstate
	if( left_click == True) then -- if left click
		do
		if (getCharacter boardstate i /= '2') then -- if mark is off
			do
				let c = setElement boardstate i '0'
				if  getCharacter board  i == '0' -- if mine
					then 
						do
							let b = changeboardstate board boardstate 399
							_boardstate $~! \x -> b
					else --if not mine
						if ( (getNumberofMines board 0  i (-1) []) == 0 ) -- if no mines, reveal the neighborhood untill this node is surrounded by nodes having atleast 1 mine next to it
							then 
								do
									let (_,b)= getAdjacentList board 0 ([i], c) 
									_boardstate $~! \x -> b
						else _boardstate $~! \x -> c
		else  do_nothing
	else if (flag == True) then -- if right click
		do
			if (getCharacter boardstate i == '2') then -- if mark is on, turn it off
				do
					let c = setElement boardstate i '1'
					_boardstate $~! \x -> c
			else -- else turn it on
				do
					let c = setElement boardstate i '2'
					_boardstate $~! \x -> c
	else do_nothing

modifyBoard::[Char] -> Integer ->  Integer -> Integer -> [Char]
modifyBoard board length index no_of_mines =
	if length == no_of_mines
		then
			board
		else
			if length > no_of_mines
				then
					if getCharacter board index == '0'
						then
							modifyBoard (setElement board index '1') (length - 1) ( (index+3) `mod` 400) no_of_mines
						else
							modifyBoard board length ( (index + 1) `mod` 400) no_of_mines
				else
					if getCharacter board index == '0'
						then 
							modifyBoard board length ( (index + 1) `mod` 400) no_of_mines
						else
							modifyBoard (setElement board index '0') (length + 1) ( (index + 3) `mod` 400) no_of_mines

do_nothing ::  IO ()
do_nothing = return ()

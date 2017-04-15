module GameHandle where

import Data.IORef
import Control.Monad
import Search
import Operations
import Graphics.UI.GLUT	

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

do_nothing ::  IO ()
do_nothing = return ()

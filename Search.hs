module Search where

import Operations

{-

getList function

arguments -> 1. index 
			 2. boardstate
			 3. integer array from which we have to select indices that are not revealed
			 4. integer array list which stores the indices which have been revealed already

this function checks if a given index is in range from (1,400) i.e. within board size

if it is in range and the index is not in the list (i.e it has not been revealed)

then it makes the boardstate of that index to revealed and appends the item in list

and calls getList recursively until we parse through the whole array l 

-}

getList:: Integer -> [Char] -> [Integer] -> [Integer] -> ([Integer] , [Char])
getList index boardstate l list =  
	if index < getZero
		then 
			(list, boardstate)
		else
			do
				let x = getInt l index
				if x >= 0 && x < 400 && if_contains list x  > 0
				 	then 
				 		getList (index-1) (setElement boardstate x '0') l (append list x)
				 	else
				 		getList (index - 1) boardstate l list


{-
	
getAdjacentList 

arguments : 1. board
			2. index
			3. tuple of list  and boardstate

this function checks if the given index has 0 number of mines in its neighbourhood

and it puts all 8 neighbouring nodes in a list and calls getList

then it retrieves the list and boardstate from getlist and calls itself recursively

until we reach the end of list
-}


getAdjacentList :: [Char] -> Integer -> ([Integer],[Char]) -> ([Integer], [Char])
getAdjacentList board index (list, boardstate) = do

	if index == getLength list
		then (list, boardstate)
		else
			if getNumberofMines board 0 (getInt list index) (-1) [] < 1
				then 
					do 
						let m = getInt list index
						if m `mod` 20 == 0
							then
								do
									let cols = 20
									let l = [m+1,m-cols,m-cols+1,m+cols,m+cols+1,(-1),(-1),(-1)]
									getAdjacentList board (index + 1) (getList 7 boardstate l list)
							else
								if m `mod` 20 == 19
									then
										do
											let cols = 20
											let l = [m-1,m-cols,m-cols-1,m+cols,m+cols-1,(-1),(-1),(-1)]
											getAdjacentList board (index+1) (getList 7 boardstate l list)
									else
										do
											let cols = 20
											let l =  [m - cols,m+cols,m - cols-1,m - cols+1,m+cols+1,m +cols -1 , m - 1 , m + 1]	
											getAdjacentList board (index +1 ) (getList 7 boardstate l list)
				else
					getAdjacentList board (index + 1) (list,  boardstate)


{-
getNumberofMines 

arguments : 1. board
			2. result (which is initialized with 0)
			3. root index for which we want to know number of mines in neighbourhood
			4. initialized index to (-1)
			5. integer array which contains all nodes in the neighbourhood of root

this function recurses until we have gone through each node in neighbourhood of root

and if a node is mine then it increments result by 1 and calls itself recursively by incrementing index
-}


getNumberofMines:: [Char] -> Integer -> Integer -> Integer -> [Integer]  -> Integer
getNumberofMines board result root index list = 

	if index == -1
		then 
			if root `mod` 20 == 0
				then
					do
						let cols = 20
						let l = [root + 1,root + cols , root - cols , root - cols + 1, root + cols + 1,(-1),(-1),(-1)]
						getNumberofMines board result root 0 l
				else
					if root `mod` 20 == 19 
						then
							do
								let cols = 20
								let l = [root -1 , root + cols , root + cols -1 ,root - cols,root - cols -1,(-1),(-1),(-1)]
								getNumberofMines board result root 0 l
						else
							do
								let cols = 20
								let l = [root+1,root-1,root+cols+1,root+cols-1,root+cols,root-cols-1,root-cols+1,root-cols]
								getNumberofMines board result root 0 l
		else
			if index == 8
				then 
					result
				else

					if getInt list index >= 0 && getInt list index < 400 
						then
							if getCharacter board (getInt list index ) == '0'
								then 
									getNumberofMines board (result + 1) root (index +1) list 
								else
									getNumberofMines board result root (index + 1) list
						else
							getNumberofMines board result root (index + 1) list

{-
if_contains

arguments 1. list in which we want to Search for
		  2. element which we are Searching for

returns 1 if list is empty

if the first element matches then returns 0

else recursively call with tail of the list and element as arguments 
-}

if_contains	:: [Integer] -> Integer  -> Integer
if_contains [] _ = 1
if_contains (x:xs) element  =
	if x == element 
		then 
			0
		else
			if_contains xs element
	


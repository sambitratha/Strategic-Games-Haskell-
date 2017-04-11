append :: [Int] -> Int -> [Int]
append [] val = [val]
append (x:xs) val = x:(append xs val)

getCharacter :: [Char] -> Int -> Char
getCharacter (x:xs) 0 = x 
getCharacter (x:xs) index = getCharacter xs (index - 1)

getInt::[Int] -> Int -> Int
getInt (x:xs) 0 = x 
getInt (x:xs) index = getInt xs (index - 1)

setElement::[Char] -> Int -> Char -> [Char]
setElement (x:xs) 0 val = val:xs
setElement (x:xs) index val = x:(setElement xs (index - 1) val)

getZero :: Int
getZero = 0
--getList 0 boardstate l list = (list, boardstate)
getList:: Int -> [Char] -> [Int] -> [Int] -> ([Int] , [Char])
getList index boardstate l list =  
	if index < getZero
		then 
			(list, boardstate)
		else
			do
				let x = getInt l index
				if x >= 0 && x < 2500 && if_contains list x (length list) > 0
				 	then 
				 		getList (index-1) (setElement boardstate x '0') l (append list x)
				 	else
				 		getList (index - 1) boardstate l list

getAdjacentList :: [Char] -> Int -> ([Int],[Char]) -> ([Int], [Char])
getAdjacentList board index (list, boardstate) = do

	if index == length(list)
		then (list, boardstate)
		else
			if getNumberofMines board 0 (getInt list index) (-1) [] < 1
				then 
					do
						let l = [index - 50,index+50,index - 51,index - 49,index+51,index + 49 , index - 1 , index + 1]		
						getAdjacentList board (index + 1) (getList 7 boardstate l list) 
				else

					getAdjacentList board (index + 1) (list,  boardstate)


getNumberofMines:: [Char] -> Int -> Int -> Int -> [Int] -> Int
getNumberofMines board result root index list = 

	if index == -1
		then 
			do
				let l = [root+1,root-1,root+5,root+6,root+4,root-4,root-5,root-6]
				getNumberofMines board result root 0 l
		else
			if index == 8
				then result
				else

					if getInt list index >= 0 && getInt list index < 2500 
						then
							if getCharacter board (getInt list index ) == '0'
								then 
									getNumberofMines board (result + 1) root (index +1) list
								else
									getNumberofMines board result root (index + 1) list
						else
							getNumberofMines board result root (index + 1) list


if_contains	:: [Int] -> Int -> Int -> Int
if_contains (x:xs) element index =
	if index == 0 
		then
			if getInt (x:xs) 0 == element
				then 0
			else 1

		else
			if getInt (x:xs) index == element 
				then 0
				else
					if_contains xs element (index + 1)


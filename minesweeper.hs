import Graphics.UI.GLUT	
import Data.IORef
import System.Random
import Control.Monad

import Operations -- does the basic list manipulations and list generations 
import Glut_window_fns -- has most of the functions that use glut tools
import GameHandle -- Has functions that implement the game rules
import Search -- has implementation for BFS search algorithm to explore neighboring locations

display :: IORef Bool -> IORef Bool -> IORef Position -> IORef [Char] -> IORef [Char] -> IORef Bool ->  Integer -> DisplayCallback
display leftclick flag pos _boardstate _board gameover no_of_mines= do  -- the callback function for display
	clear [ColorBuffer]
	loadIdentity
	(Position x1 y1) <- readIORef pos
	board <- readIORef _board
	boardstate <- readIORef _boardstate
	_flag <- readIORef flag
	_leftclick <- readIORef leftclick
	let gamecheck = (checkifwon board boardstate 399)
	game_over <- readIORef gameover
	if( game_over == False) then gameover $~! \x -> gamecheck
	else do_nothing
	let (x', y') = de_normalize (normalize_plz (x1,y1))
	if game_over == True && _leftclick == True then -- if gameover and left click command to reset has been received
		do
			let a = create_board_state 400
			_boardstate $~! \x -> a
			let s = False
			gameover $~! \x -> s
			seed <- newStdGen
			let c = initBoard seed 400
			let a = modifyBoard c (getTotalMines c 400 0) 0 no_of_mines
			_board $~! \x -> a
	else if ( (x1 /= 0 || y1/=0)  && (getCharacter boardstate ( ((x'-1)*20 + (y'-1))) == '1' || getCharacter boardstate ( ((x'-1)*20 + (y'-1)))== '2')) 
	    		then
	    			do
	    				modify_boardstate _leftclick _flag _board _boardstate (toInteger ( ( (x'-1)*20 + (y'-1))) )
	    	else 	do_nothing
	forM_ (points 20 20) $ \(x,y,z) -> -- for everypoint in the list generated by points function
	    preservingMatrix $ do
	    	color $ Color3 (x-x) (y-y) (z-z)
	    	draw_line ( (x-0.05), -1) ( (x-0.05), 1)
	    	translate $ Vector3 x y 0
	    	let (x2, y2) = de_normalize (x,y)
	    	let game_over = False
	    	boardstate <- readIORef _boardstate
	    	if (getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0' && (getCharacter board (((x2-1)*20 +(y2-1))) == '0')) then -- choose the color of the tile to be drawn
	    		do
	    			color $ Color3 (x-x+1.0) (y-y) (z-z)
	    			let s = True
	    			gameover $~! \x -> s
	    	else if (getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0') then color $ Color3 (x-x+0.95) (y-y+0.95) (z-z+0.95)
	    	else if ( getCharacter boardstate (((x2-1)*20 +(y2-1))) == '2' ) then color $ Color3 (x-x) (y-y+1.0) (z-z)
	    	else	color $ Color3 (x-x+0.65) (y-y+0.65) (z-z+0.65)
	    	cube 0.05 -- draw the tile
	    	if ( getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0' && (getCharacter board (((x2-1)*20 +(y2-1))) /= '0') ) then -- if explored and not flagged, reveal the number of mines in it
	    		do
	    			translate $ Vector3 (-x) (-y) 0
	    			color $ Color3 (x-x) (y-y) (z-z+1.0 )
	    			draw_num (getNumberofMines board 0 (((x2-1)*20 +(y2-1))) (-1) []) (x,y)
	    			translate $ Vector3 x y 0
	    	else do_nothing
	    	translate $ Vector3 (-x) (-y) 0
	    	color $ Color3 (x-x) (y-y) (z-z)
	    	draw_line ( -1, y+0.05) ( 1, y+0.05)
	    	translate $ Vector3 x y 0
	let s = False
	flag $~! \x -> s 
	leftclick $~! \x -> s
	flush
main :: IO ()
main = do
	(_progName, _args) <- getArgsAndInitialize
	initialWindowSize $= Size 600 600
	putStrLn "Enter the total number of mines (between 0-400) : "
	no_of_mines <- readLn :: IO Integer -- variable for number of mines
	_window <- createWindow "MineSweeper"-- variable for window
	pos <- newIORef (Position 0 0) -- variable for the mouse location when the last click was performed
	_boardstate <- newIORef ['0'] -- variable for boardstate -> a list that says if at an index, the tile is revealed / marked / not revealed yet
	_board <- newIORef ['0'] -- variable for board -> a list that says if at an index, the tile contains a mine or not
	_leftclick <- newIORef False -- if left click has been performed at the latest
	_flag <- newIORef False -- if right clck has been performed at the latest
	game_over <- newIORef False -- if the game is over ( to reset )
	seed <- newStdGen -- a variable to generate a random list
	let a = create_board_state 400 -- a list which says every index is not revealed yet
	let c = initBoard seed 400 -- a list that has a random number of mines
	let	b = modifyBoard  c (getTotalMines c 400 0) 0  no_of_mines -- a modified list that has a fixed number of mines
	_boardstate $~! \x -> a 
	_board $~! \x -> b
	reshapeCallback $= Just  reshape
	keyboardMouseCallback $= Just (keyboardMouse _leftclick _flag pos)
	displayCallback $= display _leftclick _flag pos _boardstate _board game_over no_of_mines
	idleCallback $= Just idle
	mainLoop


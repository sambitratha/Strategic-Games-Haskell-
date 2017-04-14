import Graphics.UI.GLUT	
import Data.IORef
import System.Random
import Control.Monad

import Operations
import Glut_window_fns
import GameCreation
import Search

display :: IORef Bool -> IORef Bool -> IORef Position -> IORef [Char] -> IORef [Char] -> IORef Bool ->  DisplayCallback
display leftclick flag pos boardstate1 board1 gameover= do 
	clear [ColorBuffer]
	loadIdentity
	(Position x1 y1) <- readIORef pos
	board <- readIORef board1
	boardstate <- readIORef boardstate1
	_flag <- readIORef flag
	_leftclick <- readIORef leftclick
	let gamecheck = (checkifwon board boardstate 399)
	game_over <- readIORef gameover
	if( game_over == False) then gameover $~! \x -> gamecheck
	else stalker 5
	let (x', y') = de_normalize (normalize_plz (x1,y1))
	if game_over == True && _leftclick == True then
		do
			let a = create_board_state 400
			boardstate1 $~! \x -> a
			let s = False
			gameover $~! \x -> s
			seed <- newStdGen
			let a = initBoard seed 400
			board1 $~! \x -> a
	else if ( (x1 /= 0 || y1/=0)  && (getCharacter boardstate ( ((x'-1)*20 + (y'-1))) == '1' || getCharacter boardstate ( ((x'-1)*20 + (y'-1)))== '2')) 
	    		then
	    			do
	    				modify_boardstate _leftclick _flag board1 boardstate1 (toInteger ( ( (x'-1)*20 + (y'-1))) )
	    	else 	stalker 5
	forM_ (points 20 20) $ \(x,y,z) ->
	    preservingMatrix $ do
	    	color $ Color3 (x-x) (y-y) (z-z)
	    	draw_line ( (x-0.05), -1) ( (x-0.05), 1)
	    	translate $ Vector3 x y 0
	    	let (x2, y2) = de_normalize (x,y)
	    	let game_over = False
	    	boardstate <- readIORef boardstate1
	    	if (getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0' && (getCharacter board (((x2-1)*20 +(y2-1))) == '0')) then
	    		do
	    			color $ Color3 (x-x+1.0) (y-y) (z-z)
	    			let s = True
	    			gameover $~! \x -> s
	    	else if (getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0') then color $ Color3 (x-x+0.95) (y-y+0.95) (z-z+0.95)
	    	else if ( getCharacter boardstate (((x2-1)*20 +(y2-1))) == '2' ) then color $ Color3 (x-x) (y-y+1.0) (z-z)
	    	else	color $ Color3 (x-x+0.65) (y-y+0.65) (z-z+0.65)
	    	cube 0.05
	    	if ( getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0' && (getCharacter board (((x2-1)*20 +(y2-1))) /= '0') ) then
	    		do
	    			translate $ Vector3 (-x) (-y) 0
	    			color $ Color3 (x-x) (y-y) (z-z+1.0 )
	    			draw_num (getNumberofMines board 0 (((x2-1)*20 +(y2-1))) (-1) []) (x,y)
	    			translate $ Vector3 x y 0
	    	else stalker 5
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
	_window <- createWindow "MineSweeper"
	pos <- newIORef (Position 0 0)
	boardstate1 <- newIORef ['0']
	board1 <- newIORef ['0']
	_leftclick <- newIORef False
	_flag <- newIORef False
	game_over <- newIORef False
	seed <- newStdGen
	let a = create_board_state 400
	let	b = initBoard seed 400
	boardstate1 $~! \x -> a
	board1 $~! \x -> b
	reshapeCallback $= Just reshape
	keyboardMouseCallback $= Just (keyboardMouse _leftclick _flag pos)
	displayCallback $= display _leftclick _flag pos boardstate1 board1 game_over
	idleCallback $= Just idle
	mainLoop


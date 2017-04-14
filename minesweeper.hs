import Graphics.UI.GLUT	
import Control.Monad
import Data.IORef
import System.Random
import BFS


append1 :: [Char] -> [Char] -> [Char]
append1 [] [] = []
append1 [] (x:xs) = (x:xs)
append1 (x:xs) [] = (x:xs)
append1 (x:xs) (y:ys) = x:(append1 xs (y:ys))

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

points :: Int  -> Int -> [(GLfloat, GLfloat, GLfloat)]
points n 1 = [ (-0.95, (0.95-((fromIntegral (k-1))*0.1)) , 0) | k <- [1..n'] ]
	where n' = fromIntegral n
points n m = (points n (m-1)) ++ [ ((-0.95 + (fromIntegral (m-1))*0.1), (0.95-(fromIntegral (k-1))*0.1), 0) | k <- [1..n'] ] 
	where n' = fromIntegral n

normalize_plz :: (GLint, GLint) -> (GLfloat, GLfloat)
normalize_plz (x,y) = ((fromIntegral (toInteger x)-150)/150, (-(fromIntegral (toInteger y)-150)/150))
	
create_board_state :: Int -> [Char]
create_board_state n = ['1' | k <- [1..n']]
	where n' = fromIntegral n

create_board ::Int -> [Char] ->[Char]
create_board 0 board = board
create_board len board = create_board (len - 1) (append1 board "11111111011000001010")

de_normalize :: ( GLfloat, GLfloat) -> (Integer, Integer)
de_normalize (x,y) = ( 1+floor (((1-y)/0.1)) ,1+floor (((x + 1) /0.1)) )


stalker :: Int -> IO ()
stalker 5 = return ()

modify_boardstate :: Bool -> IORef [Char] -> IORef [Char] -> Integer -> IO ()
modify_boardstate flag board1 boardstate1 i = do
	board <- readIORef board1
	boardstate <- readIORef boardstate1
	if( flag == False) then
		do
		let c = setElement boardstate (toInteger i) '0'
		if  getCharacter board  (toInteger i) == '0' 
			then 
				do
					let a = create_board 20 ""
					board1 $~! \x -> a
					let b = create_board_state 400
					boardstate1 $~! \x -> b
			else 
				if ( (getNumberofMines board 0 (toInteger i) (-1) []) == 0 )
					then 
						do
							let (_,b)= getAdjacentList board 0 ([(toInteger i)], c) 
							boardstate1 $~! \x -> b
				else boardstate1 $~! \x -> c
	else do
			let c = setElement boardstate (toInteger i) '2'
			boardstate1 $~! \x -> c

draw_num :: Integer -> ( GLfloat, GLfloat) -> IO()
draw_num no (x,y) = do 
	case no of
		1 -> do
				draw_line ( x+0.025, y-0.025)  ( x+ 0.025 , y+0.025)
		2 -> do
				draw_line ( x-0.025, y+0.025) ( x+0.025, y+0.025)
				draw_line ( x-0.025, y-0.025) ( x+0.025, y-0.025)
				draw_line ( x-0.025, y) ( x+0.025, y)
				draw_line ( x-0.025, y-0.025) ( x-0.025, y)
				draw_line ( x+0.025, y+0.025) ( x+0.025, y)
		3 -> do
				draw_line ( x+0.025, y+0.025) ( x+0.025 , y-0.025)
				draw_line ( x-0.025, y) ( x+0.025 , y)
				draw_line ( x-0.025, y+0.025) ( x+0.025 , y+0.025)
				draw_line ( x-0.025, y-0.025) ( x+0.025 , y-0.025)
		4 -> do
				draw_line ( x-0.025, y) ( x+0.025, y+0.025)
				draw_line ( x + 0.025, y+0.025) ( x + 0.025 , y - 0.025)
				draw_line (x + 0.025 , y) ( x- 0.025, y)
		5 -> do
				draw_line ( x-0.025, y+0.025) ( x+0.025, y+0.025)
				draw_line ( x-0.025, y-0.025) ( x+0.025, y-0.025)
				draw_line ( x-0.025, y) ( x+0.025, y)
				draw_line ( x-0.025, y+0.025) ( x-0.025, y)
				draw_line ( x+0.025, y-0.025) ( x+0.025, y)
		6 -> do
				draw_line ( x-0.025, y) ( x+0.025 , y)
				draw_line ( x-0.025, y+0.025) ( x+0.025 , y+0.025)
				draw_line ( x-0.025, y-0.025) ( x+0.025 , y-0.025)
				draw_line ( x+0.025, y-0.025) ( x+0.025, y)
				draw_line ( x-0.025, y-0.025)  ( x- 0.025 , y+0.025)
		7 -> do
				draw_line ( x+0.025, y+0.025) ( x+0.025 , y-0.025)
				draw_line ( x-0.025, y+0.025) ( x+0.025, y+0.025)
		8 -> do
				draw_line ( x-0.025, y) ( x+0.025 , y)
				draw_line ( x-0.025, y+0.025) ( x+0.025 , y+0.025)
				draw_line ( x-0.025, y-0.025) ( x+0.025 , y-0.025)
				draw_line ( x+0.025, y-0.025) ( x+0.025, y + 0.025)
				draw_line ( x-0.025, y-0.025)  ( x- 0.025 , y+0.025)
		_ -> return ()

display :: IORef Bool -> IORef Position -> IORef [Char] -> IORef [Char] ->  DisplayCallback
display flag pos boardstate1 board1 = do 
	clear [ColorBuffer]
	loadIdentity
	(Position x1 y1) <- readIORef pos
	board <- readIORef board1
	boardstate <- readIORef boardstate1
	_flag <- readIORef flag
	let (x', y') = de_normalize (normalize_plz (x1,y1))
	if ( (x1 /= 0 || y1/=0)  && getCharacter boardstate ( ((x'-1)*20 + (y'-1))) == '1') 
	    		then
	    			do
	    				modify_boardstate _flag board1 boardstate1 (toInteger ( ( (x'-1)*20 + (y'-1))) )
	    	else 	stalker 5
	forM_ (points 20 20) $ \(x,y,z) ->
	    preservingMatrix $ do
	    	color $ Color3 (x-x) (y-y) (z-z)
	    	draw_line ( (x-0.05), -1) ( (x-0.05), 1)
	    	translate $ Vector3 x y 0
	    	let (x2, y2) = de_normalize (x,y)
	    	boardstate <- readIORef boardstate1
	    	if (getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0') then color $ Color3 (x-x+0.95) (y-y+0.95) (z-z+0.95)
	    	else if ( getCharacter boardstate (((x2-1)*20 +(y2-1))) == '2' ) then color $ Color3 (x-x) (y-y+1.0) (z-z)
	    	else	color $ Color3 (x-x+0.65) (y-y+0.65) (z-z+0.65)
	    	cube 0.05
	    	if ( getCharacter boardstate (((x2-1)*20 +(y2-1))) == '0') then
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
	    	-- need to print numbers :l
	flush

idle :: IdleCallback
idle  = do
	postRedisplay Nothing

keyboardMouse :: IORef Bool -> IORef Position -> KeyboardMouseCallback
keyboardMouse flag p key Down _ pos = case key of
	(MouseButton LeftButton) -> p $~! \x -> pos
	(MouseButton RightButton) -> do
									p $~! \x -> pos
									let s = True
									flag $~! \x -> s
	_ -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

main :: IO ()
main = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "MineSweeper"
	pos <- newIORef (Position 0 0)
	boardstate1 <- newIORef ['0']
	board1 <- newIORef ['0']
	_flag <- newIORef False
	let a = create_board_state 400
	let	b = create_board 20 ""
	boardstate1 $~! \x -> a
	board1 $~! \x -> b
	let s = False
	_flag $~! \x -> s
	keyboardMouseCallback $= Just (keyboardMouse _flag pos)
	displayCallback $= display _flag pos boardstate1 board1
	idleCallback $= Just idle
	mainLoop

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]

draw_line ( x, y) ( a, b) = do
	renderPrimitive  Lines $ do
		vertex $ Vertex3 x y 0
		vertex $ Vertex3 a b 0


module Glut_window_fns where

import Data.IORef
import Graphics.UI.GLUT	

keyboardMouse :: IORef Bool -> IORef Bool -> IORef Position -> KeyboardMouseCallback -- callback for keyboardmouse inputs
keyboardMouse _leftclick flag p key Down _ pos = case key of
	(MouseButton LeftButton) ->	do 					-- if left click
									p $~! \x -> pos -- modify click position
									let s = True
									_leftclick $~! \x -> s
	(MouseButton RightButton) -> do 				-- if right click 
									p $~! \x -> pos -- modify click position
									let s = True
									flag $~! \x -> s
	_ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO () -- a shorthand to call a Vertex3 constructor 
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cube :: GLfloat -> IO () -- creates a cube of edge length w
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]


draw_num :: Integer -> ( GLfloat, GLfloat) -> IO() 
draw_num no (x,y) = do -- draws the number no inputted in an LED display format
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


draw_line :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO() -- draws a line taking the 2 points as input
draw_line ( x, y) ( a, b) = do
	renderPrimitive  Lines $ do
		vertex $ Vertex3 x y 0
		vertex $ Vertex3 a b 0

reshape :: ReshapeCallback -- reshapes the window and modifies the size parameter of window
reshape size = do 
	viewport $= (Position 0 0, size)
	Graphics.UI.GLUT.windowSize $= size

idle :: IdleCallback -- idlecallback, basically does nothing
idle  = do
	postRedisplay Nothing


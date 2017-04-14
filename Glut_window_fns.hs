module Glut_window_fns where

import Data.IORef
import Graphics.UI.GLUT	

keyboardMouse :: IORef Bool -> IORef Bool -> IORef Position -> KeyboardMouseCallback
keyboardMouse _leftclick flag p key Down _ pos = case key of
	(MouseButton LeftButton) ->	do
									p $~! \x -> pos
									let s = True
									_leftclick $~! \x -> s
	(MouseButton RightButton) -> do
									p $~! \x -> pos
									let s = True
									flag $~! \x -> s
	_ -> return ()
keyboardMouse _ _ _ _ _ _ _ = return ()

vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z

cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]


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


draw_line :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> IO()
draw_line ( x, y) ( a, b) = do
	renderPrimitive  Lines $ do
		vertex $ Vertex3 x y 0
		vertex $ Vertex3 a b 0

reshape :: ReshapeCallback
reshape size = do 
	viewport $= (Position 0 0, size)
	Graphics.UI.GLUT.windowSize $= size

idle :: IdleCallback
idle  = do
	postRedisplay Nothing


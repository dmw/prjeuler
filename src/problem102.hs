----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  https://github.com/dmw/prjeuler
-- Repository  :  https://github.com/dmw/prjeuler
--
-- Project Euler, Problem 102.
--
-- Three distinct points are plotted at random on a Cartesian plane,
-- for which -1000  x, y  1000, such that a triangle is formed.
--
-- Consider the following two triangles:
--
-- A(-340,495), B(-153,-910), C(835,-947)
--
-- X(-175,41), Y(-421,-714), Z(574,-645)
--
-- It can be verified that triangle ABC contains the origin, whereas
-- triangle XYZ does not.
--
-- Using triangles.txt (right click and 'Save Link/Target As...'), a
-- 27K text file containing the co-ordinates of one thousand "random"
-- triangles, find the number of triangles for which the interior
-- contains the origin.
--
-- NOTE: The first two examples in the file represent the triangles in
-- the example given above.
-----------------------------------------------------------------------------


module Main (main) where


import Data.List.Split
import Data.String.Utils (strip)
import Graphics.Rendering.OpenGL
import qualified Graphics.Rendering.FTGL as F
import Graphics.UI.GLUT
import System.Environment
import System.Exit
import Text.Printf


-- | Point Representation as Tuple.
data LPt = LPt (Int, Int)
         deriving (Eq, Show, Ord)

-- | Triangle Reprentation.
data LTrg = LTrg {
  tA        :: LPt      -- ^ Point A
  , tB      :: LPt      -- ^ Point B
  , tC      :: LPt      -- ^ Point C
  } deriving (Eq, Show, Ord)


-- | Instance for the LPt data type as Num
instance Num LPt where
  -- | Defines the (+) operation for the LPt data type
  x + y = LPt (fstp x + fstp y, sndp x + sndp y)

  -- | Defines the (-) operation for the LPt data type
  x - y = LPt (fstp x - fstp y, sndp x - sndp y)

  -- | Defines the (*) operation for the LPt data type
  x * y = LPt (fstp x * fstp y, sndp x * sndp y)

  -- | Defines the abs function for the LPt data type
  abs x = LPt (abs (fstp x), abs (sndp x))

  -- | Defines the fromInteger function for the LPt data type
  fromInteger x = LPt (fromInteger x, fromInteger x)

  -- | Defines the signum functon for the LPt data type
  signum x | fstp x < 0 && sndp x < 0 = -1
           | fstp x > 0 && sndp x > 0 = 1
           | fstp x == sndp x = 0
  signum _ = 0


-- | Calculates the Cross Product for the given pair
-- of points.
crossPt :: LPt          -- ^ Point A
           -> LPt       -- ^ Point B
           -> LPt       -- ^ Resulting Point
crossPt x y = LPt (0, fstp x * sndp y - sndp x * fstp y)


-- | Constant Zero Axis in OpenGL
zidxZero :: GLfloat
zidxZero = 0.0 :: GLfloat


-- | Color Cyan
colorCyan :: IO ()
colorCyan = color $ Color3 0 (1.0 :: GLfloat) (1.0 :: GLfloat)


-- | Color Green
colorGreen :: IO ()
colorGreen = color $ Color3 0 (1.0 :: GLfloat) 0

-- | Point Zero (in the middle of the screen)
zeroPt :: LPt
zeroPt = LPt (0, 0)

-- | X Axis of the Point
fstp :: LPt             -- ^ Point to get the X Axis.
        -> Int          -- ^ Returning Axis.
fstp (LPt (x, y)) = x

-- | Y Axis of the Point
sndp :: LPt             -- ^ Point to extract the Y Axis.
        -> Int          -- ^ Returning Axis.
sndp (LPt (x, y)) = y

-- | OpenGL location of the Point of the 1000.0 x 1000.0 plane.
ptGlF :: LPt -> (GLfloat, GLfloat)
ptGlF p = ((fromIntegral (fstp p) / 1000.0) :: GLfloat,
           (fromIntegral (sndp p) / 1000.0) :: GLfloat)


-- | LPt to OpenGL vertex converter.
ptToGlPt :: LPt         -- ^ Point to Convert.
            -> IO ()    -- ^ OpenGL Vertex.
ptToGlPt p = vertex $ uncurry Vertex3 (ptGlF p) zidxZero


-- | Creates a Triangle based on the given string of points,
-- where it should have 6 integers delimited by comma.
mkTriangle :: String    -- ^ String to Convert.
              -> LTrg   -- ^ Resulting Triangle.
mkTriangle s = LTrg { tA = head g, tB = g !! 1, tC = last g }
  where g = fmap (\x -> LPt (head x, last x) )
            $ splitEvery 2 ln
        ln = fmap (read . strip) $ splitOn "," s

-- | Determines the color of the triangle based on its position
-- if it is containing the point zero.
trgColor :: LTrg        -- ^ Point to Check
            -> IO ()    -- ^ OpenGL Color
trgColor xs | trgContained zeroPt xs = colorCyan
trgColor xs = colorGreen

-- | Displays a text text in the OpenGL screen.
displayText :: String           -- ^ Text to display.
               -> F.Font        -- ^ Font to be used.
               -> IO ()         -- ^ Returning OpenGL Text.
displayText text font = do
  F.renderFont font text F.All
  flush

-- | Renders the sumarizing result text on the OpenGL screen
-- using the triangle xs and the given font.
trgTextCont :: [LTrg]           -- ^ Triangles to Check.
               -> F.Font        -- ^ Font to be used.
               -> IO ()         -- ^ Rendered OpenGL text.
trgTextCont xs = displayText text
                 where text = printf "Cont.: %d | Not Cont.: %d" cnts cnts2
                       cnts2 = length xs - cnts
                       cnts = length $ filter (trgContained zeroPt) xs

-- | Builds the triangle list using the string input s, but using
-- the limits l and h to slice the final list.
mkTriangles :: String           -- ^ String to Parse.
               -> Int           -- ^ Lower limit.
               -> Int           -- ^ Upper Limit.
               -> [LTrg]        -- ^ Triangle List.
mkTriangles s l h = drop l
                    $ take h
                    $ fmap mkTriangle
                    $ filter (\ x -> length x > 0 )
                    $ lines s


-- | Checks if the given point p is contained in the given triangle t.
trgContained :: LPt             -- ^ Point to Check.
                -> LTrg         -- ^ Triangle to Check.
                -> Bool         -- ^ True if it is contained.
trgContained p t = sndp crs1 >= 0 && sndp crs2 >= 0 && sndp crs3 >= 0
                   || sndp crs1 <= 0 && sndp crs2 <= 0 && sndp crs3 <= 0
  where crs1 = crossPt seg1 (p - tA t)
        crs2 = crossPt seg2 (p - tB t)
        crs3 = crossPt seg3 (p - tC t)
        seg1 = tB t - tA t
        seg2 = tC t - tB t
        seg3 = tA t - tC t

-- | Displays a Triangle on the OpenGL screen.
displayTriangle :: LTrg         -- ^ Triangle to be displayed.
                   -> IO ()     -- ^ Rendered OpenGL Triangle.
displayTriangle xs = do
  renderPrimitive Polygon
    $ do trgColor xs
         mapM_ ptToGlPt [tA xs, tB xs, tC xs]
  flush

-- | Displays a list of triangles and sumarizing text using
-- the given list xs and font f.
displayTriangles :: [LTrg]      -- ^ List of Triangles.
                    -> F.Font   -- ^ Font to be used.
                    -> IO ()    -- ^ Renderized OpengGL.
displayTriangles xs f = do
  clear [ ColorBuffer, DepthBuffer ]
  mapM_ displayTriangle xs
  trgTextCont xs f
  flush

-- | Handles IOError execptions.
handlerIOError :: IOError       -- ^ Error to handle.
                  -> IO ()      -- ^ Error message.
handlerIOError e = putStrLn (printf "IOError: %s" $ show e)
                   >> putStrLn (printf "Usage: problem102 triangles.txt low-bound hi-bound")
                   >> exitFailure


-- | Main OpenGL Function.
mainGl :: IO ()
mainGl = do
  [x,y,z] <- getArgs
  inp <- readFile x
  (progname, _) <- getArgsAndInitialize
  putStrLn $ printf "Running %s" progname
  loadIdentity
  initialWindowSize $= Size 800 800
  _ <- createWindow "Triangles Display"
  matrixMode $= Projection
  polygonMode $= (Line, Line)
  font <- F.createPixmapFont "aller.ttf"
  _ <- F.setFontFaceSize font 24 24
  _ <- F.setFontDepth font 1.0
  displayCallback $= displayTriangles (mkTriangles inp (read y) (read z)) font
  mainLoop


-- | Main Function.
main :: IO ()
main = mainGl `catch` handlerIOError




module Main (main) where


import Data.List.Split
import Data.String.Utils (strip)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Environment
import System.IO


data LPt = LPt (Int, Int)
         deriving (Eq, Show, Ord)

data LTrg = LTrg {
  tA        :: LPt
  , tB      :: LPt
  , tC      :: LPt
  } deriving (Eq, Show, Ord)


instance Num LPt where
  x + y = LPt (fstp x + fstp y, sndp x + sndp y)

  x - y = LPt (fstp x - fstp y, sndp x - sndp y)

  x * y = LPt (fstp x * fstp y, sndp x * sndp y)

  abs x = LPt (abs (fstp x), abs (sndp x))

  fromInteger x = LPt (fromInteger x, fromInteger x)

  signum x | fstp x < 0 && sndp x < 0 = -1
  signum x | fstp x > 0 && sndp x > 0 = 1
  signum x = 0


crossPt :: LPt -> LPt -> LPt
crossPt x y = LPt (0, fstp x * sndp y - sndp x * fstp y)

zidxZero :: GLfloat
zidxZero = 0.0 :: GLfloat

zeroPt :: LPt
zeroPt = LPt (0, 0)

fstp :: LPt -> Int
fstp (LPt (x, y)) = x

sndp :: LPt -> Int
sndp (LPt (x, y)) = y

ptGlF :: LPt -> (GLfloat, GLfloat)
ptGlF p = ((fromIntegral (fstp p) / 1000.0) :: GLfloat,
           (fromIntegral (sndp p) / 1000.0) :: GLfloat)

ptToGlPt :: LPt -> IO ()
ptToGlPt p = vertex $ uncurry Vertex3 (ptGlF p) zidxZero

mkTriangle :: String -> LTrg
mkTriangle s = LTrg { tA = head g, tB = g !! 1, tC = last g }
               where g = fmap (\x -> LPt (head x, last x) )
                         $ splitEvery 2 ln
                     ln = fmap (read . strip) $ splitOn "," s

trgColor :: LTrg -> IO ()
trgColor xs = if trgContained zeroPt xs
                 then color $ Color3 0 0 (1.0 :: GLfloat)
              else color $ Color3 0 (0.25 :: GLfloat) 0

displayTriangle :: LTrg -> IO ()
displayTriangle xs = do renderPrimitive Polygon $ do
                          trgColor xs
                          mapM ptToGlPt [tA xs, tB xs, tC xs]
                        flush

displayTriangles :: [LTrg] -> IO ()
displayTriangles xs = do clear [ ColorBuffer, DepthBuffer ]
                         mapM_ displayTriangle xs

mkTriangles :: String -> Int -> Int -> [LTrg]
mkTriangles s l h = drop l
                    $ take h
                    $ fmap mkTriangle
                    $ filter (\ x -> length x > 0 )
                    $ lines s

trgContained :: LPt -> LTrg -> Bool
trgContained p t = sndp crs1 >= 0 && sndp crs2 >= 0 && sndp crs3 >= 0
                   || sndp crs1 <= 0 && sndp crs2 <= 0 && sndp crs3 <= 0
                   where crs1 = crossPt seg1 (p - tA t)
                         crs2 = crossPt seg2 (p - tB t)
                         crs3 = crossPt seg3 (p - tC t)
                         seg1 = tB t - tA t
                         seg2 = tC t - tB t
                         seg3 = tA t - tC t

main :: IO ()
main = do [x,y,z] <- getArgs
          inp <- readFile x
          (progname, _) <- getArgsAndInitialize
          loadIdentity
          initialWindowSize $= Size 800 800
          createWindow "Trimatrix Display"
          matrixMode $= Projection
          polygonMode $= (Line, Line)
          displayCallback $= displayTriangles (mkTriangles inp (read y) (read z))
          mainLoop



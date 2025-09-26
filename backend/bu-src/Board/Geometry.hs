module Board.Geometry where

-- TODO: specialize pragmas for the below
indexToCoord :: Integral i => i -> (i, i)
indexToCoord = swap . flip divMod 11

coordToIndex :: Integral i => (i, i) -> i
coordToIndex (x, y) = (y * 11) + x

centerCoord :: Integral i => (i, i) -> (i, i)
centerCoord (x, y) = (x - 5, y - 5)

uncenterCoord :: Integral i => (i, i) -> (i, i)
uncenterCoord (x, y) = (x + 5, y + 5)

rotate90 :: Integral i => (i, i) -> (i, i)
rotate90 (x, y) = (y, -x)

rotate180 :: Integral i => (i, i) -> (i, i)
rotate180 (x, y) = (-x, -y)

rotate270 :: Integral i => (i, i) -> (i, i)
rotate270 (x, y) = (-y, x)

flipVertical :: Integral i => (i, i) -> (i, i)
flipVertical (x, y) = (x, -y)

flipHorizontal :: Integral i => (i, i) -> (i, i)
flipHorizontal (x, y) = (-x, y)

flipDiagonalSouthEast :: Integral i => (i, i) -> (i, i)
flipDiagonalSouthEast (x, y) = (x, y)

flipDiagonalSouthWest :: Integral i => (i, i) -> (i, i)
flipDiagonalSouthWest (x, y) = (-x, -y)

withCenteredCoord :: Integral i => ((i, i) -> (i, i)) -> i -> i
withCenteredCoord f i =
  coordToIndex $ uncenterCoord $ f $ centerCoord $ indexToCoord i

rotateIndex90 :: Integral i => i -> i
rotateIndex90 = withCenteredCoord rotate90

rotateIndex180 :: Integral i => i -> i
rotateIndex180 = withCenteredCoord rotate180

rotateIndex270 :: Integral i => i -> i
rotateIndex270 = withCenteredCoord rotate270

flipIndexVertical :: Integral i => i -> i
flipIndexVertical = withCenteredCoord flipVertical

flipIndexHorizontal :: Integral i => i -> i
flipIndexHorizontal = withCenteredCoord flipHorizontal

flipIndexSouthEast :: Integral i => i -> i
flipIndexSouthEast = withCenteredCoord flipDiagonalSouthEast

flipIndexSouthWest :: Integral i => i -> i
flipIndexSouthWest = withCenteredCoord flipDiagonalSouthWest

module Util.Geometry where

-- Represents a vector in 2-space
data Vector = Vector !Float !Float deriving (Eq, Ord, Show)

-- The sum of two vectors
vAdd :: Vector -> Vector -> Vector
vAdd (Vector ux uy) (Vector vx vy) = Vector (ux + vx) (uy + vy)

-- The difference of two vectors
vSub :: Vector -> Vector -> Vector
vSub (Vector ux uy) (Vector vx vy) = Vector (ux - vx) (uy - vy)

-- Scales the components of a vector independently
scale :: Vector -> (Float, Float) -> Vector
scale (Vector x y) (kx, ky) = Vector (x * kx) (y * ky)

-- Scales both components of a vector uniformly
scaleUniform :: Vector -> Float -> Vector
scaleUniform u k = scale u (k, k)

-- The squared magnitude of a vector
sqMag :: Vector -> Float
sqMag (Vector x y) = x * x + y * y

-- The magnitude of a vector
mag :: Vector -> Float
mag = sqrt . sqMag

-- The squared distance between two points
sqDist :: Vector -> Vector -> Float
sqDist u v = sqMag $ vSub u v

-- The distance between two points
dist :: Vector -> Vector -> Float
dist u v = mag $ vSub u v

-- Normalizes a vector
norm :: Vector -> Vector
norm u = scaleUniform u (1.0 / mag u)

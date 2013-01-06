module Vector2
( Vector2(..)
, vector2Add
, vector2Sub
, vector2Scale
, vector2Length
, vector2LengthSquare
, vector2Unit
, vector2Dot
, vector2Projection
, vector2Perpendicular
, vector2Show
) where

type Vector2 = (Double, Double)

vector2Add :: Vector2 -> Vector2 -> Vector2
vector2Add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

vector2Sub :: Vector2 -> Vector2 -> Vector2
vector2Sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

vector2Scale :: Vector2 -> Double -> Vector2
vector2Scale (x, y) s = (s * x, s * y)

vector2Length :: Vector2 -> Double
vector2Length (x, y) = sqrt (x*x + y*y)

vector2LengthSquare :: Vector2 -> Double
vector2LengthSquare (x, y) = (x*x + y*y)

vector2Unit :: Vector2 -> Vector2
vector2Unit vec@(x, y) =
    let length = vector2Length vec
    in (x / length, y / length)

vector2Dot :: Vector2 -> Vector2 -> Double
vector2Dot (x1, y1) (x2, y2) = (x1 * x2) + (y1 * y2)

vector2Projection :: Vector2 -> Vector2 -> Vector2
vector2Projection v1@(x1, y1) v2@(x2, y2) =
    let dot = vector2Dot v1 v2
        v2LengthSquare = vector2LengthSquare v2
    in v2 `vector2Scale` (dot / v2LengthSquare)

vector2Perpendicular :: Vector2 -> Vector2 -> Vector2
vector2Perpendicular v1@(x1, y1) v2@(x2, y2) =
    let proj = vector2Projection v1 v2
    in v1 `vector2Sub` proj

vector2Show :: Vector2 -> String
vector2Show v@(x, y) = show x ++ ", " ++ show y

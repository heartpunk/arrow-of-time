import Color exposing (..)
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import List exposing (map)
import Math.Vector2 exposing (..)
import Signal exposing (..)
import Random exposing (generate, list, pair, float, int)

type Wall = Left | Right | Top | Bottom
type Collision = DiscCollision DiscRecord DiscRecord | WallCollision Wall

type alias Model = List DiscRecord

type alias DiscRecord =
  { x        : Float
  , y        : Float
  , vx       : Float
  , vy       : Float
  , collided : Bool
  , id       : Int
--  , collisions : List Collision
  }

collageSize : Float
collageSize = 500

discSize : Float
discSize = 10

discRadius : Float
discRadius = (discSize / 2)

upperBound : Float
upperBound = (collageSize / 2) - 7

lowerBound : Float
lowerBound = -upperBound

truncateToBounds : Float -> Float -> Float -> Float
truncateToBounds upper lower a = if a > upper then upper else (if a < lower then lower else a)

collidedWithWall : Float -> Float -> Bool
collidedWithWall x vx = (x > (upperBound - discRadius)) || (x < (lowerBound))

newPos : Float -> Float -> Float
newPos x vx =
  if (collidedWithWall x vx)
  then (truncateToBounds (upperBound - discRadius) (lowerBound + discRadius) x) + vx
  else x + vx

newVel : Float -> Float -> Float
newVel x vx = if (collidedWithWall x vx) then -vx else vx

reflectDisc : Vec2 -> Vec2 -> Vec2
reflectDisc vel vectorPerpendicularToWall =
  let
    d      = vel
    twiceD = Math.Vector2.scale 2 d
    newVel = sub d (Math.Vector2.scale ((dot twiceD vectorPerpendicularToWall )/(lengthSquared vectorPerpendicularToWall)) vectorPerpendicularToWall)
  in
    newVel

memberBy : (a -> a -> Bool) -> a -> List a -> Bool
memberBy f x = List.any (f x)

uniqueBy : (a -> a -> Bool) -> List a -> List a
uniqueBy f things =
  let
    core seen f (x::xs) =
      if xs == []
      then []
      else (
            if memberBy f x seen
            then core seen f xs
            else x :: (core (x::seen) f xs))
  in
    core [] f things

reflectionVector : Collision -> Math.Vector2.Vec2
reflectionVector collision =
  let
    rotate v = (vec2 -(getY v) (getX v))
  in
    case collision of
      WallCollision Left -> vec2 -1 -1
      WallCollision Right -> vec2 1 1
      WallCollision Top -> vec2 1 1
      WallCollision Bottom -> vec2 -1 -1
      DiscCollision a b -> (rotate << Math.Vector2.negate) (direction (vec2 a.x a.y) (vec2 b.x b.y))

newVelocityVector : Vec2 -> List Collision -> Math.Vector2.Vec2
newVelocityVector vec collisions = List.foldl reflectDisc vec (List.map reflectionVector collisions)

updateRecord : List (DiscRecord, DiscRecord) -> DiscRecord -> DiscRecord
updateRecord currentCollidedPairs discRecord =
  let
    currentDiscCollisions = discCollisions currentCollidedPairs discRecord
    currentWallCollisions = wallCollisions discRecord
    currentCollisions     = currentDiscCollisions ++ currentWallCollisions
    collided              = (not << List.isEmpty) currentCollisions
    vel                   = vec2 discRecord.vx discRecord.vy
    newVx                 = getX (newVelocityVector vel currentCollisions)
    newVy                 = getY (newVelocityVector vel currentCollisions)
  in
    { discRecord |
      x        <- newPos discRecord.x newVx,
      y        <- newPos discRecord.y newVy,
      vx       <- newVx,
      vy       <- newVy,
      collided <- collided --,
--      collisions <- currentCollisions
    }

discCollisions : List (DiscRecord, DiscRecord) -> DiscRecord -> List Collision
discCollisions currentCollidedPairs discRecord =
  let
    otherDisc (a,b) =
    if | equalById discRecord a -> Just b
       | equalById discRecord b -> Just a
       | otherwise              -> Nothing
    collisions = List.filterMap otherDisc currentCollidedPairs
  in
    List.map2 DiscCollision (List.repeat (List.length collisions) discRecord) collisions

--collidedWithWall x vx = (x > (upperBound - discRadius)) || (x < (lowerBound))

wallCollisions : DiscRecord -> List Collision
wallCollisions disc =
  let
    core p side = if p then [WallCollision side] else []
    left   = core (disc.x < lowerBound + discRadius) Left
    right  = core (disc.x > (upperBound - discRadius)) Right
    top    = core (disc.y > (upperBound - discRadius)) Top
    bottom = core (disc.y < lowerBound + discRadius) Bottom
  in
    left ++ right ++ top ++ bottom

--wallVector : Math.Vector2.Vec2
--wallVector = vec2 2.25 0.5
--
--collidePair : (DiscRecord,DiscRecord) -> (DiscRecord,DiscRecord)
--collidePair (a,b) = (reflectDisc a wallVector, reflectDisc b wallVector)

ourFlatten : (List a, List a) -> List a
ourFlatten (a,b) = a ++ b

equalById : DiscRecord -> DiscRecord -> Bool
equalById a b = a.id == b.id

update : Float -> Model -> Model
update dt model =
  let
    currentCollidedPairs = collidedPairs (pairs model)
--    updatePair (a,b)     = (updateRecord currentCollidedPairs a, updateRecord currentCollidedPairs b)
--    postCollisionDiscs   = (ourFlatten << List.unzip << List.map (collidePair << updatePair)) currentCollidedPairs
--  in
--    uniqueBy equalById (postCollisionDiscs ++ (
  in
     List.map (updateRecord currentCollidedPairs) model

disc : Color -> Form
disc color =
  group
    [ filled color (circle discSize)
    , outlined (solid grey) (circle discSize)
      ]


-- this assumes a list of unique items.
pairs : List a -> List (a,a)
pairs items =
  let
    core (x::xs) pairsSoFar =
      if   xs == []
      then pairsSoFar
      else core xs ( (List.map ( \y-> (x,y) ) xs) ++ pairsSoFar)
  in
    core items []

collidedPairs : List (DiscRecord, DiscRecord) -> List (DiscRecord, DiscRecord)
collidedPairs =
  let -- don't do square root
    squaredDistance a b = (a.x - b.x)^2 + (a.y-b.y)^2
    squaredMinDistance  = (discRadius * 4)^2
    pairCollided (a,b)  = (squaredDistance a b) < squaredMinDistance
  in
    List.filter pairCollided

view : Model -> Element
view model =
  let
    color collided = if collided then red else lightGrey
    renderedDisc discRecord = move (discRecord.x, discRecord.y) (disc (color discRecord.collided))
  in
    collage (truncate collageSize) (truncate collageSize) ( (List.map renderedDisc model) ++ [outlined (solid red) (rect collageSize collageSize)])

defaultDisc : DiscRecord
defaultDisc =
  { x        = 200
  , y        = 200
  , vx       = -5
  , vy       = 5
  , collided = False
  , id       = 1
--  , collisions = []
  }

defaultModel : Model
defaultModel =
  let
    count                               = 50
    randomCoordComponent                = float lowerBound upperBound
    generateAndDiscardNewSeed generator = fst (generate generator (Random.initialSeed 0))
    randomPositions                     = generateAndDiscardNewSeed (list count (pair randomCoordComponent randomCoordComponent))
    randomVelocityComponent             = float -20 20
    randomVelocities                    = generateAndDiscardNewSeed (list count (pair randomVelocityComponent randomVelocityComponent))
    triple a b c                        = (a,b,c)
    randomVelocitiesAndPositions        = List.map3 triple randomPositions randomVelocities [1..count]
    offsetDisc ((x,y),(vx,vy),i) =
      { defaultDisc |
        x  <- x
      , y  <- y
      , vx <- vx
      , vy <- vy
      , id <- defaultDisc.id * (truncate i)
      }
  in
    List.map offsetDisc randomVelocitiesAndPositions

main : Signal Element
main =
  Signal.map view (Debug.watch "model" <~ (Signal.foldp update defaultModel (fps 120)))

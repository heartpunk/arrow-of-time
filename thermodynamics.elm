module Thermodynamics where

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
  }

collageSize : Float
collageSize = 900

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
newPos x vx = (truncateToBounds (upperBound - discRadius) (lowerBound + discRadius) x) + vx

reflectDisc : Vec2 -> Vec2 -> Vec2
reflectDisc vel vectorPerpendicularToWall =
  let
    d         = vel
    magnitude = length vel
    twiceD    = Math.Vector2.scale 2 d
    newVel    = sub d (Math.Vector2.scale ((dot twiceD vectorPerpendicularToWall )/(lengthSquared vectorPerpendicularToWall)) vectorPerpendicularToWall)
  in
   Math.Vector2.scale (1 * magnitude) (normalize newVel)

reflectionVector : Vec2 -> Collision -> Math.Vector2.Vec2
reflectionVector vec collision =
  let
    rotate v = (vec2 -(getY v) (getX v))
    magnitude = length vec
    newVec x y = (Math.Vector2.scale magnitude) (vec2 x y)
  in
    case collision of
      WallCollision Left   -> newVec  1  0
      WallCollision Right  -> newVec -1  0
      WallCollision Top    -> newVec  0 -1
      WallCollision Bottom -> newVec  0  1
      DiscCollision a b    -> (rotate << Math.Vector2.negate) (sub (vec2 a.x a.y) (vec2 b.x b.y))

newVelocityVector : Vec2 -> List Collision -> Math.Vector2.Vec2
newVelocityVector vec collisions =
  List.foldl reflectDisc vec (List.map (reflectionVector vec) collisions)

currentFps : Float
currentFps = 15

idealTickLength : Float
idealTickLength = ( 1000 / currentFps )

currentTickOverIdeal : Float -> Float
currentTickOverIdeal dt = (Time.inMilliseconds dt) / idealTickLength

updateRecord : Float -> List (DiscRecord, DiscRecord) -> DiscRecord -> DiscRecord
updateRecord dt currentCollidedPairs discRecord =
  let
    currentDiscCollisions = discCollisions currentCollidedPairs discRecord
    currentWallCollisions = wallCollisions discRecord
    currentCollisions     = currentDiscCollisions ++ currentWallCollisions
    collided              = (not << List.isEmpty) currentCollisions
    vel                   = Math.Vector2.scale dt (toVelocityVector discRecord)
    newVel                = newVelocityVector (toVelocityVector discRecord) currentCollisions
    scaledNewVel          = Math.Vector2.scale ( dt / currentFps ) newVel
    scaledNewVx           = getX scaledNewVel
    scaledNewVy           = getY scaledNewVel
    collisionVelocityCoefficient = 20
  in
    { discRecord |
      x        <- newPos discRecord.x ( (if collided then collisionVelocityCoefficient else 1) * scaledNewVx ),
      y        <- newPos discRecord.y ( (if collided then collisionVelocityCoefficient else 1) * scaledNewVy ),
      vx       <- getX newVel,
      vy       <- getY newVel,
      collided <- collided
    }

equalById : DiscRecord -> DiscRecord -> Bool
equalById a b = a.id == b.id

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

nFix : Int -> (a -> a) -> a -> a
nFix n f a = if n > 0 then nFix (n-1) f (f a) else a

update : Float -> Model -> Model
update dt model =
  let
    currentCollidedPairs = collidedPairs (pairs model)
    core dt model = List.map (updateRecord dt currentCollidedPairs) model
    iterationCount = 300
  in
     nFix iterationCount (core (dt/iterationCount)) model

disc : Color -> Vec2 -> Form
disc color vec =
  group
    [ filled color (circle discSize)
    , outlined (solid grey) (circle discSize)
    , traced (solid black) (segment (toTuple (Math.Vector2.scale -1 vec)) (toTuple (Math.Vector2.scale 3 vec)))
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
    color collided          = if collided then red else lightGrey
    renderedDisc discRecord = move (discRecord.x, discRecord.y) (disc (color discRecord.collided) (vec2 discRecord.vx discRecord.vy))
  in
    collage (truncate collageSize) (truncate collageSize) ( (List.map renderedDisc model) ++ [outlined (solid red) (rect collageSize collageSize)])

defaultDisc : DiscRecord
defaultDisc =
  { x        = 0
  , y        = 0
  , vx       = -1
  , vy       = 1
  , collided = False
  , id       = 1
  }

defaultModel : Model
defaultModel =
  let
    count                               = 150
    randomCoordComponent                = float lowerBound upperBound
    generateAndDiscardNewSeed generator = fst (generate generator (Random.initialSeed 0))
    randomPositions                     = generateAndDiscardNewSeed (list count (pair randomCoordComponent randomCoordComponent))
    maxVelocity                         = 20
    randomVelocityComponent             = float -maxVelocity maxVelocity
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
  Signal.map view (Debug.watch "model" <~ (Signal.foldp update defaultModel (Debug.watch "dt" <~ ( Signal.map currentTickOverIdeal (Debug.watch "fps" <~ (fps currentFps) ) ) )))

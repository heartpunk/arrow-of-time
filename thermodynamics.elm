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
  , color : Color
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
reflectDisc vectorPerpendicularToWall vel =
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

-- this should be DiscRecord -> List Collision -> Math.Vector2.Vec2
newVelocityVector : Vec2 -> List Collision -> Math.Vector2.Vec2
newVelocityVector vec collisions =
  List.foldl reflectDisc vec (List.map (reflectionVector vec) collisions)

currentFps : Float
currentFps = 10

idealTickLength : Float
idealTickLength = ( 1000 / currentFps )

currentTickOverIdeal : Float -> Float
currentTickOverIdeal dt = (Time.inMilliseconds dt) / idealTickLength

toPositionVector : DiscRecord -> Math.Vector2.Vec2
toPositionVector dr = vec2 dr.x dr.y

toVelocityVector : DiscRecord -> Math.Vector2.Vec2
toVelocityVector dr = vec2 dr.vx dr.vy

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
    collisionVelocityCoefficient = 5
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
    iterationCount = 10
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
    color dr          = if dr.collided  then red else dr.color
    renderedDisc discRecord = move (discRecord.x, discRecord.y) (disc (color discRecord) (Math.Vector2.scale 0.2 (vec2 discRecord.vx discRecord.vy) ))
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
  , color = lightGrey
  }

flatten : List (a, a) -> List a
flatten ((x1, x2)::xs) = [x1,x2] ++ if List.isEmpty xs then [] else flatten xs

collided : List DiscRecord -> List DiscRecord
collided discs = flatten (collidedPairs (pairs discs ) )

isCollided : List DiscRecord -> DiscRecord -> Bool
isCollided discs disc = not ( List.member disc (collided discs) )

defaultModel : Model
defaultModel =
  let
    count                               = 150
    randomCoordComponent                = float lowerBound upperBound
    generateAndDiscardNewSeed generator = fst (generate generator (Random.initialSeed 0))
    randomRedComponent = int 0 200
    randomRedComponents = generateAndDiscardNewSeed ( list count randomRedComponent )
    randomBlueComponent = int 0 255
    randomBlueComponents = generateAndDiscardNewSeed ( list count randomRedComponent )
    randomGreenComponent = int 0 255
    randomGreenComponents = generateAndDiscardNewSeed ( list count randomGreenComponent )
    triple a b c = (a,b,c)
    randomColors = List.map3 triple randomRedComponents randomBlueComponents randomGreenComponents
    randomPositions                     = generateAndDiscardNewSeed (list count (pair randomCoordComponent randomCoordComponent))
    maxVelocity                         = 50
    minVelocity = maxVelocity * 0.5
    randomVelocitySign = int 0 1
    randomVelocitySigns = generateAndDiscardNewSeed (list count (pair randomVelocitySign randomVelocitySign))
    randomVelocityComponent             = float minVelocity maxVelocity
    randomVelocities                    = generateAndDiscardNewSeed (list count (pair randomVelocityComponent randomVelocityComponent))
    quadruple a b c d                        = (a,b,c,d)
    quintuple a b c d e                      = (a,b,c,d,e)
    randomVelocitiesAndPositionsAndColors = List.map5 quintuple randomPositions randomVelocities randomVelocitySigns [1..count] randomColors
    offsetDisc ((x,y),(vx,vy),(sx,sy),i,(r,g,b)) =
      { defaultDisc |
        x  <- x
      , y  <- y
      , vx <- if sx == 0 then -vx else vx
      , vy <- if sy == 0 then -vy else vy
      , id <- defaultDisc.id * (truncate i)
      , color <- rgb r g b
      }
    core = List.map offsetDisc randomVelocitiesAndPositionsAndColors
    nonCollided = List.filter (isCollided core) core
  in
    nonCollided

main : Signal Element
main =
  Signal.map view (Debug.watch "model" <~ (Signal.foldp update defaultModel (Debug.watch "dt" <~ ( Signal.map currentTickOverIdeal (Debug.watch "fps" <~ (fps currentFps) ) ) )))

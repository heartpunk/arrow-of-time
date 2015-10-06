import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Time exposing (..)
import List exposing (map)


-- WHAT
zip : List a -> List b -> List (a,b)
zip xs ys =
  case (xs, ys) of
    ( x :: xs', y :: ys' ) ->
        (x,y) :: zip xs' ys'

    (_, _) ->
        []


type alias Model = List DiscRecord

type alias DiscRecord =
  { x        : Float
  , y        : Float
  , vx       : Float
  , vy       : Float
  , collided : Bool
  }

collageSize : Float
collageSize = 500

discSize : Float
discSize = 10

discRadius : Float
discRadius = (discSize / 2) + 5

upperBound : Float
upperBound = collageSize / 2

lowerBound : Float
lowerBound = -upperBound

truncateToBounds : Float -> Float -> Float -> Float
truncateToBounds upper lower a = if a > upper then upper else (if a < lower then lower else a)

collided : Float -> Float -> Bool
collided x vx = (x > (upperBound - discRadius)) || (x < (lowerBound))

newPos : Float -> Float -> Float
newPos x vx =
  if (collided x vx)
  then (truncateToBounds (upperBound - discRadius) (lowerBound + discRadius) x)
  else x + vx

newVel : Float -> Float -> Float
newVel x vx = if (collided x vx) then -vx else vx

update : Float -> Model -> Model
update dt model =
  let currentCollidedPairs    = collidedPairs (pairs model)
      discCollided discRecord =
        not (
             List.isEmpty (
                           List.filter
                                 (\(a,b) -> (a == discRecord) || (b == discRecord))
                                 currentCollidedPairs
                          )
            )
      updatedRecord discRecord =
        { discRecord |
          x        <- newPos discRecord.x discRecord.vx,
          y        <- newPos discRecord.y discRecord.vy,
          vx       <- newVel discRecord.x discRecord.vx,
          vy       <- newVel discRecord.y discRecord.vy,
          collided <- discCollided discRecord
        }
  in map updatedRecord model

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
      else core xs ( (map ( \y-> (x,y) ) xs) ++ pairsSoFar)
  in
    core items []

collidedPairs : List (DiscRecord, DiscRecord) -> List (DiscRecord, DiscRecord)
collidedPairs =
  let -- don't do square root
    distance a b       = sqrt((a.x - b.x)^2 + (a.y-b.y)^2)
    pairCollided (a,b) = (distance a b) < ((discRadius * 2))
  in
    List.filter pairCollided

view : Model -> Element
view model =
  let
    color collided = if collided then red else lightGrey
    renderedDisc discRecord = move (discRecord.x, discRecord.y) (disc (color discRecord.collided))
  in
    collage 500 500 ( (map renderedDisc model) ++ [outlined (solid red) (rect 500 500)])

defaultDisc : DiscRecord
defaultDisc =
  { x        = 0
  , y        = 0
  , vx       = 2
  , vy       = -1
  , collided = False
  }

defaultModel : Model
defaultModel =
  let
    offsetDisc i =
      { defaultDisc |
        x <- i * 20 + defaultDisc.x,
        y <- i * 20 + defaultDisc.y,
        vx <- defaultDisc.vx + (0.2*i),
        vy <- defaultDisc.vy - (0.3*i)
      }
  in
    map offsetDisc [1..10]

main : Signal Element
main =
  Signal.map view (Signal.foldp update defaultModel (fps 120))

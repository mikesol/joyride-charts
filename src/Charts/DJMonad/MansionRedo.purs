module Charts.DJMonad.MysteryMansionMadnessRedo
  ( Events
  , OneTwoThreeFour(..)
  , asInt
  , basic
  , basic'
  , basic''
  , basic1111
  , basic1234
  , basic1313
  , basic2341
  , basic2424
  , basic3131
  , basic3412
  , basic4123
  , basic4242
  , cascade1122
  , cascade2233
  , cascade3344
  , cascade4411
  , earliest
  , groove
  , groove2
  , integrate
  , intro
  , intro'beat
  , laugh
  , mbToTime
  , mbsToTime
  , noDice
  , outro
  , piece
  , rise
  , shift'
  , shift''
  , squeeze
  , squeezeFromHead
  , suspense
  , tempo
  , validateEvents
  ) where

-- | # Mystery Mansion Madness
-- |
-- | By DJ Monad
-- |
-- | | 1 - 5  | 5 - 9        | 9 - 17 | 17 - 19  | 19 - 21 | 21 - 29 | 29 - 37 | 37 - 45 |
-- | | Intro  | Intro + Beat | Rise   | Suspense | Laugh   | Groove  | Groove2 | Outro   |

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (nubBy, sortBy)
import Data.Array as Array
import Data.Function (on)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Validation.Semigroup (V, invalid, validation)
import Effect.Exception (Error, error)
import Foreign (ForeignError(..), MultipleErrors)
import SimpleTypes (Column, Smpl, farLeft, farRight, nearLeft, nearRight, overOne, overThree, overTwo)

data OneTwoThreeFour = One | Two | Three | Four

derive instance Eq OneTwoThreeFour
derive instance Ord OneTwoThreeFour

asInt :: OneTwoThreeFour -> Int
asInt One = 1
asInt Two = 2
asInt Three = 3
asInt Four = 4

tempo :: Int
tempo = 140

mbToTime :: Int -> OneTwoThreeFour -> Number
mbToTime m b = mbsToTime m b 0.0

mbsToTime :: Int -> OneTwoThreeFour -> Number -> Number
mbsToTime m b s = (toNumber ((integrate (m - 1) + (asInt b - 1))) + s) * 60.0 / (toNumber tempo)

integrate :: Int -> Int
integrate m = m * 4

shift' :: Int -> OneTwoThreeFour -> Smpl -> Smpl
shift' i b = shift $ toNumber ((i * 4) + asInt b)

shift'' :: Int -> Smpl -> Smpl
shift'' i = shift $ toNumber (i * 4)

shift :: Number -> Smpl -> Smpl
shift n' = go
  where
  n = n' * 60.0 / toNumber tempo
  go e = e
    { timing = e.timing + n
    }

earliest :: Array Smpl -> Number
earliest = go top
  where
  go x = Array.uncons >>> case _ of
    Nothing -> x
    Just { head: e, tail } -> go (min x e.timing) tail

toColumn :: (Column -> Column) -> Smpl -> Smpl
toColumn f = go
  where
  go e = e
    { column = f e.column
    }

squeeze :: Number -> Number -> Smpl -> Smpl
squeeze n refn = go
  where
  go e = e
    { timing = (e.timing - refn) * n + refn
    }

basic1313 :: Int -> Column -> Array Smpl
basic1313 i = basic i One Three One Three

basic2424 :: Int -> Column -> Array Smpl
basic2424 i = basic i Two Four Two Four

basic3131 :: Int -> Column -> Array Smpl
basic3131 i = basic i Three One Three One

basic4242 :: Int -> Column -> Array Smpl
basic4242 i = basic i Four Two Four Two

basic1234 :: Int -> Column -> Array Smpl
basic1234 i = basic i One Two Three Four

basic1111 :: Int -> Column -> Array Smpl
basic1111 i = basic i One One One One

basic2341 :: Int -> Column -> Array Smpl
basic2341 i = basic i Two Three Four One

basic3412 :: Int -> Column -> Array Smpl
basic3412 i = basic i Three Four One Two

basic4123 :: Int -> Column -> Array Smpl
basic4123 i = basic i Four One Two Three

cascade1122 :: Int -> Column -> Array Smpl
cascade1122 i c = basic'' i One 0.0 i One 0.5 i Two 0.0 i Two 0.5 c
  <> basic'' i One 0.5 i Two 0.0 i Two 0.5 i Three 0.0 (overOne c)
  <> basic'' i Two 0.0 i Two 0.5 i Three 0.0 i Three 0.5 (overTwo c)
  <> basic'' i Two 0.5 i Three 0.0 i Three 0.5 i Four 0.0 (overThree c)

cascade2233 :: Int -> Column -> Array Smpl
cascade2233 i c = map (shift 1.0) $ cascade1122 i c

cascade3344 :: Int -> Column -> Array Smpl
cascade3344 i c = map (shift 2.0) $ cascade1122 i c

cascade4411 :: Int -> Column -> Array Smpl
cascade4411 i c = map (shift 3.0) $ cascade1122 i c

basic :: Int -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> Column -> Array Smpl
basic m1 b1 b2 b3 b4 = basic' m1 b1 m2 b2 m3 b3 m4 b4
  where
  m2 = if b2 <= b1 then m1 + 1 else m1
  m3 = if b3 <= b2 then m2 + 1 else m2
  m4 = if b4 <= b3 then m3 + 1 else m3

basic'' :: Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Column -> Array Smpl
basic'' m1 b1 s1 _m2 _b2 _s2 _m3 _b3 _s3 _m4 _b4 _s4 column = pure $
  { timing: mbsToTime m1 b1 s1
  , column
  }

basic' :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Column -> Array Smpl
basic' m1 b1 m2 b2 m3 b3 m4 b4 = basic'' m1 b1 0.0 m2 b2 0.0 m3 b3 0.0 m4 b4 0.0

squeezeFromHead :: Number -> Array Smpl -> Array Smpl
squeezeFromHead n l = map (squeeze n stt) l
  where
  stt = earliest l

intro :: Array Smpl
intro = join
  [ basic1111 1 nearRight
  , basic1111 2 farLeft
  , basic1111 3 nearLeft
  , basic1111 4 farRight
  ]

intro'beat :: Array Smpl
intro'beat = join
  [ basic3412 5 nearRight
  , basic3412 6 nearRight
  , basic3412 7 nearRight
  , basic3412 8 farLeft
  , basic3412 8 farLeft
  ]

rise :: Array Smpl
rise =
  join
    [ basic1234 9 nearLeft
    , basic1234 9 nearRight
    , basic2341 9 nearLeft
    , basic2341 9 farRight
    , basic3412 9 nearLeft
    , basic3412 9 farRight
    , basic4123 9 nearLeft
    , basic4123 9 farRight
    , basic1234 10 nearLeft
    , basic1234 10 farRight
    , basic2341 10 nearLeft
    , basic2341 10 farRight
    , basic3412 10 nearLeft
    , basic3412 10 farRight
    , basic4123 10 nearLeft
    , basic4123 10 farRight
    , basic1234 11 nearLeft
    , basic1234 11 farRight
    , basic1234 11 farLeft
    , basic1234 11 farLeft
    , basic2341 11 nearLeft
    , basic2341 11 farRight
    , basic2341 11 farLeft
    , basic2341 11 farLeft
    , basic3412 11 nearLeft
    , basic3412 11 farRight
    , basic3412 11 farLeft
    , basic3412 11 farLeft
    , basic4123 11 nearLeft
    , basic4123 11 farRight
    , basic4123 11 farLeft
    , basic4123 11 farLeft
    --
    , basic1234 12 nearLeft
    , basic1234 12 farRight
    , basic1234 12 farLeft
    , basic1234 12 farLeft
    , basic2341 12 nearLeft
    , basic2341 12 farRight
    , basic2341 12 farLeft
    , basic2341 12 farLeft
    , basic3412 12 farLeft
    , basic3412 12 farLeft
    , basic4123 12 nearLeft
    , basic4123 12 farRight
    , basic1234 13 nearLeft
    , basic1234 13 farRight
    , basic2341 13 farLeft
    , basic2341 13 farLeft
    , basic3412 13 nearLeft
    , basic3412 13 farRight
    , basic3412 13 farRight
    , basic3412 13 nearLeft
    , basic4123 13 nearRight
    , basic4123 13 nearRight
    --
    , basic1234 14 nearLeft
    , basic1234 14 farRight
    , basic2341 14 farLeft
    , basic2341 14 farLeft
    , basic3412 14 farRight
    , basic3412 14 nearLeft
    , basic3412 14 nearLeft
    , basic3412 14 farRight
    , basic4123 14 nearRight
    , basic4123 14 nearRight
    ]
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic1234 15 nearLeft
            , basic1234 15 farRight
            , basic2341 15 farLeft
            , basic2341 15 farLeft
            , basic3412 15 farRight
            , basic3412 15 nearLeft
            , basic4123 15 nearRight
            , basic4123 15 nearRight
            ]
        )
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic3412 15 nearLeft
            , basic3412 15 farRight
            , basic4123 15 farLeft
            , basic4123 15 farLeft
            , basic1234 16 farRight
            , basic1234 16 nearLeft
            , basic2341 16 nearRight
            , basic2341 16 nearRight
            ]
        )
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic1234 16 nearLeft
            , basic1234 16 farRight
            , basic2341 16 farLeft
            , basic2341 16 farLeft
            , basic3412 16 farRight
            , basic3412 16 nearLeft
            , basic4123 16 nearRight
            , basic4123 16 nearRight
            ]
        )
    <>
      --
      squeezeFromHead 0.5
        ( join
            [ basic3412 16 nearLeft
            , basic3412 16 farRight
            , basic4123 16 farLeft
            , basic4123 16 farLeft
            , basic1234 17 farRight
            , basic1234 17 nearLeft
            , basic2341 17 nearRight
            , basic2341 17 nearRight
            ]
        )
    <>
      --
      join
        [ -- something here?
        ]

suspense :: Array Smpl
suspense = join
  [ basic1111 17 farLeft
  , basic1111 18 farRight
  ]

laugh :: Array Smpl
laugh = join
  [ basic1111 19 farLeft
  , basic1234 19 nearLeft
  , basic1111 20 farRight
  , basic1234 20 nearRight
  ]

groove :: Array Smpl
groove = go <> map (shift'' 4) go <>
  join
    [ -- ugh
    ]
  where
  go =
    join
      [ basic1234 21 nearLeft
      , basic1234 21 farRight
      , basic2341 21 nearLeft
      , basic2341 21 farRight
      , basic3412 21 nearRight
      , basic4123 21 nearLeft
      , basic4123 21 farRight
      --
      , basic1234 22 farLeft
      , basic1234 22 nearLeft
      , basic1234 22 farRight
      , basic1234 22 farLeft
      , basic2341 22 nearLeft
      , basic2341 22 farRight
      , basic2341 22 farLeft
      , basic2341 22 farLeft
      , basic3412 22 nearRight
      , basic4123 22 nearLeft
      , basic4123 22 farRight
      , basic4123 22 farLeft
      , basic4123 22 farLeft
      --
      , basic1234 23 nearLeft
      , basic2341 23 nearLeft
      , basic2341 23 nearRight
      , basic3412 23 nearLeft
      , basic3412 23 nearRight
      , basic3412 23 farRight
      , basic4123 23 nearLeft
      , basic4123 23 nearRight
      , basic4123 23 farRight
      , basic4123 23 farLeft
      ] <> squeezeFromHead 0.5
      ( join
          [ basic1234 24 nearRight
          , basic2341 24 farRight
          , basic3412 24 farLeft
          , basic4123 24 nearLeft
          , basic1234 25 nearRight
          , basic2341 25 farRight
          , basic3412 25 farLeft
          , basic4123 25 nearLeft
          ]
      )

groove2 :: Array Smpl
groove2 = map (shift'' 8) (map (toColumn overTwo) groove <> map (toColumn overThree) groove)

outro :: Array Smpl
outro = join
  [ basic1111 37 nearRight
  , basic1111 38 nearLeft
  , basic1111 39 farRight
  , basic1111 40 nearRight
  , basic1111 41 nearLeft
  , basic1111 41 farRight
  , basic1111 42 nearRight
  , basic1111 42 nearLeft
  , basic1111 43 farRight
  , basic1111 43 nearRight
  , basic1111 44 nearLeft
  , basic1111 44 farRight
  , basic1111 45 nearLeft
  ]

type Events = V MultipleErrors (Array Smpl)

noDice :: forall res. Int -> String -> V MultipleErrors res
noDice i s = invalid $ pure (ErrorAtIndex i (ForeignError s))

validateEvents :: Array Smpl -> Events
validateEvents = traverseWithIndex \_i be -> pure be

piece :: forall m. MonadThrow Error m => m (Array Smpl)
piece = do
  let events' = intro <> intro'beat <> rise <> suspense <> laugh <> groove <> groove2 <> outro
  let ePSILON = 0.05
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    ( nubBy (\a b -> if (abs (a.timing - b.timing) < ePSILON) && (a.column == b.column) then EQ else compare a b) $ sortBy
        ( compare `on` _.timing
        )
        events
    )
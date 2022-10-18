module Charts.DJMonad.MysteryMansionMadnessSmpl where

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
  , basic1111 5 nearRight
  , basic1111 6 farLeft
  , basic1111 7 nearLeft
  , basic1111 8 farRight
  , basic1111 9 nearRight
  , basic1111 10 farLeft
  , basic1111 11 nearLeft
  , basic1111 12 farRight
  , basic1111 9 farLeft
  , basic1111 10 nearLeft
  , basic1111 11 farRight
  , basic1111 12 nearRight
  , basic1111 13 nearRight
  , basic1111 14 farLeft
  , basic1111 15 nearLeft
  , basic1111 16 farRight
  , basic1111 13 farLeft
  , basic1111 14 nearLeft
  , basic1111 15 farRight
  , basic1111 16 nearRight
  ]

type Events = V MultipleErrors (Array Smpl)

noDice :: forall res. Int -> String -> V MultipleErrors res
noDice i s = invalid $ pure (ErrorAtIndex i (ForeignError s))

validateEvents :: Array Smpl -> Events
validateEvents = traverseWithIndex \_i be -> pure be

piece :: forall m. MonadThrow Error m => m (Array Smpl)
piece = do
  let events' = intro
  let ePSILON = 0.05
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    ( nubBy (\a b -> if (abs (a.timing - b.timing) < ePSILON) && (a.column == b.column) then EQ else compare a b) $ sortBy
        ( compare `on` _.timing
        )
        events
    )
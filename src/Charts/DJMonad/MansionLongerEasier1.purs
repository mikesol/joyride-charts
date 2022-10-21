module Charts.DJMonad.MysteryMansionMadnessLongerEasier1 where

-- | # Mystery Mansion Madness
-- |
-- | By DJ Monad
-- |
-- | | 3 - 11 | 11 - 19 | 19 - 27 | 27 - 29  | 29 - 31 | 31 - 39 | 39 - 47 | 47 - 55 | 55 - 63 | 63 - 71 |
-- | | Intro  | Repeat  | Rise    | Suspense | Laugh   | Groove  | Groove2 | Groove3 | Outro1  | Outro2  |

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Array (elem, filter, nubBy, sortBy)
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

-- these basics exploit the fact that only the first timing is used
-- in this version
-- the 1111 is overkill
basic1 :: Int -> Column -> Array Smpl
basic1 = basic1111

basic1' :: Int -> Column -> Array Smpl
basic1' i c = map (shift 0.5) $ basic1 i c

basic1x' :: Int -> Column -> Array Smpl
basic1x' i c = map (shift 0.25) $ basic1 i c

basic1'x :: Int -> Column -> Array Smpl
basic1'x i c = map (shift 0.75) $ basic1 i c

basic2 :: Int -> Column -> Array Smpl
basic2 = basic2424

basic2' :: Int -> Column -> Array Smpl
basic2' i c = map (shift 0.5) $ basic2 i c

basic2x' :: Int -> Column -> Array Smpl
basic2x' i c = map (shift 0.25) $ basic2 i c

basic2'x :: Int -> Column -> Array Smpl
basic2'x i c = map (shift 0.75) $ basic2 i c

basic3 :: Int -> Column -> Array Smpl
basic3 = basic3131

basic3' :: Int -> Column -> Array Smpl
basic3' i c = map (shift 0.5) $ basic3 i c

basic4 :: Int -> Column -> Array Smpl
basic4 = basic4242

basic4' :: Int -> Column -> Array Smpl
basic4' i c = map (shift 0.5) $ basic4 i c

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

-- | By DJ Monad
-- |
-- | | 3 - 11 | 11 - 19 | 19 - 27 | 27 - 29  | 29 - 31 | 31 - 39 | 39 - 47 | 47 - 55 | 55 - 63 | 63 - 71 |
-- | | Intro  | Repeat  | Rise    | Suspense | Laugh   | Groove  | Groove2 | Groove3 | Outro1  | Outro2  |

intro :: Array Smpl
intro = join
  [ basic3 3 nearRight
  , basic3 4 farLeft
  , basic3 5 nearLeft
  , basic3 6 farRight
  , basic4 6 farRight
  , basic3 7 nearRight
  , basic3 8 farLeft
  , basic3 9 nearLeft
  , basic3 10 farRight
  , basic3 10 farRight
  ]

intro'repeat :: Array Smpl
intro'repeat = join
  [ basic3 11 nearRight
  , basic3 12 farLeft
  , basic3 13 nearLeft
  , basic3 14 farRight
  , basic4 14 farRight
  , basic3 15 nearRight
  , basic3 16 farLeft
  , basic3 17 nearLeft
  , basic3 18 farRight
  , basic3 18 farRight
  ]

rise :: Array Smpl
rise = join
  [ basic1 19 farLeft
  , basic3 19 farRight
  , basic1 20 farLeft
  , basic2 20 nearLeft
  , basic3 20 nearRight
  , basic4 20 farRight
  , basic1 21 farLeft
  , basic3 21 farRight
  , basic1 22 farLeft
  , basic2 22 nearLeft
  , basic3 22 nearRight
  , basic4 22 farRight
  --
  , basic1 23 nearLeft
  , basic2 23 nearRight
  , basic3 23 nearLeft
  , basic4 23 nearRight
  , basic1 24 nearLeft
  , basic2 24 nearRight
  , basic3 24 nearLeft
  , basic4 24 nearRight
  , basic3 24 farLeft
  , basic4 24 farRight
  --
  , basic1 25 nearLeft
  , basic2 25 nearRight
  , basic3 25 nearLeft
  , basic4 25 nearRight
  , basic1 26 nearLeft
  , basic2 26 nearRight
  , basic3 26 nearLeft
  , basic4 26 nearRight
  , basic3 26 farLeft
  , basic4 26 farRight

  ]

suspense :: Array Smpl
suspense = join
  [ basic1 27 farRight
  , basic1 28 farLeft
  ]

laugh :: Array Smpl
laugh = join
  [  basic1 29 nearLeft
  , basic3 29 farRight
  , basic1 30 nearRight
  , basic3 30 farLeft
  ]

groove :: Array Smpl
groove = go <> go2
  where
  go = join
    [ basic1 31 farLeft
    , basic1 31 farRight
    , basic2 31 farLeft
    , basic2 31 farRight
    , basic3 31 nearLeft
    , basic3 31 nearRight
    , basic4 31 farRight
    , basic4 31 farLeft
    , basic4' 31 farRight
    , basic1' 32 farLeft
    , basic1' 32 farRight
    , basic2' 32 farLeft
    , basic2' 32 farRight
    , basic3 32 nearLeft
    , basic3' 32 nearRight
    , basic4 32 nearLeft
    --
    , basic1 33 farLeft
    , basic1 33 farRight
    , basic2 33 farLeft
    , basic2 33 farRight
    , basic3 33 nearLeft
    , basic3' 33 nearRight
    , basic4 33 nearLeft
    , basic4' 33 farLeft
    , basic4' 33 farRight
    , basic1' 34 farLeft
    , basic1' 34 farRight
    , basic2' 34 farLeft
    , basic2' 34 farRight
    , basic3 34 nearLeft
    , basic3' 34 nearRight
    , basic4 34 nearLeft
    , basic4' 34 nearRight
    ]
  kicks = join
    [ basic2 35 farRight
    , basic4 35 farRight
    , basic2 36 farRight
    , basic4 36 farRight
    , basic2 37 farRight
    , basic4 37 farRight
    , basic2 38 farRight
    , basic4 38 farRight
    ]
  kickTimings = map _.timing kicks
  go2 =
    ( -- eliminate any near rights on kick so that we don't have triples
      filter (\x -> x.column /= nearRight || (x.column == nearRight && not (x.timing `elem` kickTimings)))
        -- filter out far-rights and put them on the vocal "kick"
        $ filter ((_ /= farRight) <<< _.column)
        $ map (shift'' 4) go
    ) <> kicks

colRotate :: Array Smpl -> Array Smpl
colRotate = map
  ( \x -> x
      { column = if x.column == farRight then farLeft else if x.column == farLeft then nearLeft else if x.column == nearLeft then nearRight else farRight
      }
  )

groove2 :: Array Smpl
groove2 = colRotate $ map (shift'' 8) groove

groove3 :: Array Smpl
groove3 = go <> go2
  where
  go = join
    [ basic1 47 nearLeft
    , basic1' 47 nearRight
    , basic2 47 nearLeft
    , basic2' 47 nearRight
    , basic3 47 nearLeft
    , basic3' 47 nearRight
    , basic4 47 nearLeft
    , basic4' 47 nearRight
    --
    --
    , basic1 48 nearLeft
    , basic1x' 48 nearRight
    , basic1' 48 nearLeft
    , basic1'x 48 nearRight
    --
    , basic2 48 nearLeft
    , basic2x' 48 nearRight
    , basic2' 48 nearLeft
    , basic2'x 48 nearRight
    --
    , basic3 48 nearLeft
    , basic3' 48 nearRight
    , basic4 48 nearLeft
    , basic4' 48 nearRight
    --
    --
    --
    , basic1 49 farLeft
    , basic1' 49 farRight
    , basic2 49 farLeft
    , basic2' 49 farRight
    , basic3 49 farLeft
    , basic3' 49 farRight
    , basic4 49 farLeft
    , basic4' 49 farRight
    --
    , basic1 50 nearLeft
    , basic1' 50 farRight
    , basic2 50 farRight
    , basic2' 50 farLeft
    , basic3 50 farRight
    , basic3' 50 farRight
    , basic4 50 nearLeft
    , basic4' 50 nearLeft
    , basic4' 50 farRight

    ]
  kicks = join
    [ basic2 51 farRight
    , basic4 51 farRight
    , basic2 52 farRight
    , basic4 52 farRight
    , basic2 53 farRight
    , basic4 53 farRight
    , basic2 54 farRight
    , basic4 54 farRight
    ]
  kickTimings = map _.timing kicks
  go2 =
    ( -- eliminate any near rights on kick so that we don't have triples
      filter (\x -> x.column /= nearRight || (x.column == nearRight && not (x.timing `elem` kickTimings)))
        -- filter out far-rights and put them on the vocal "kick"
        $ filter ((_ /= farRight) <<< _.column)
        $ map (shift'' 4) go
    ) <> kicks

outro1 :: Array Smpl
outro1 = go <> go2
  where
  go = join
    [ basic1 55 nearLeft
    , basic2 55 nearRight
    , basic3 55 farLeft
    , basic3' 55 farLeft
    , basic4 55 farRight
    , basic1 56 nearLeft
    , basic2 56 nearRight
    , basic2' 56 nearRight
    , basic3 56 farLeft
    , basic4 56 farRight
    , basic1 57 nearLeft
    , basic1' 57 nearLeft
    , basic2 57 nearRight
    , basic3 57 farLeft
    , basic4 57 farRight
    , basic4' 57 farRight
    , basic1 58 farLeft
    , basic1 58 farRight
    , basic2 58 nearLeft
    , basic2 58 nearRight
    , basic2' 58 nearRight
    , basic3 58 nearLeft
    , basic3 58 nearRight
    , basic3' 58 nearLeft
    , basic4 58 nearLeft
    , basic4 58 nearRight
    , basic4' 58 nearRight
    ]
  go2 = colRotate $ map (shift'' 4) go

outro2 :: Array Smpl
outro2 = join
  [ basic1 63 farLeft
  , basic3 63 farRight
  , basic1 64 farLeft
  , basic3 64 farRight
  , basic1 65 nearLeft
  , basic1 65 nearRight
  , basic3 65 farLeft
  , basic3 65 farRight
  , basic1 66 nearLeft
  , basic1 66 nearRight
  , basic3 66 farRight
  , basic3 66 farLeft
  , basic4 66 farRight
  , basic4 66 farLeft
  --
  --
  , basic1 67 farLeft
  , basic3 67 farRight
  , basic1 68 farLeft
  , basic3 68 farRight
  , basic1 69 nearLeft
  , basic1 69 nearRight
  , basic3 69 farLeft
  , basic3 69 farRight
  , basic1 70 nearLeft
  , basic1 70 nearRight
  , basic3 70 farRight
  , basic3 70 farLeft
  , basic4 70 farRight
  , basic4 70 farLeft
  , basic1 71 nearRight
  , basic1 71 nearLeft
  , basic1 72 nearRight
  , basic1 72 nearLeft
  , basic1 73 nearRight
  , basic1 73 nearLeft
  ]

type Events = V MultipleErrors (Array Smpl)

noDice :: forall res. Int -> String -> V MultipleErrors res
noDice i s = invalid $ pure (ErrorAtIndex i (ForeignError s))

validateEvents :: Array Smpl -> Events
validateEvents = traverseWithIndex \_i be -> pure be

piece :: forall m. MonadThrow Error m => m (Array Smpl)
piece = do
  let events' = intro <> intro'repeat <> rise <> suspense <> laugh <> groove <> groove2 <> groove3 <> outro1 <> outro2
  let ePSILON = 0.05
  events <- validation (throwError <<< error <<< show) pure (validateEvents events')
  pure
    ( nubBy (\a b -> if (abs (a.timing - b.timing) < ePSILON) && (a.column == b.column) then EQ else compare a b) $ sortBy
        ( compare `on` _.timing
        )
        events
    )
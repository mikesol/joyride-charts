module Charts.DJMonad.MysteryMansionMadnessLongerRails where

-- | # Mystery Mansion Madness
-- |
-- | By DJ Monad
-- |
-- | | 3 - 11 | 11 - 19 | 19 - 27 | 27 - 29  | 29 - 31 | 31 - 39 | 39 - 47 | 47 - 55 | 55 - 63 | 63 - 71 |
-- | | Intro  | Repeat  | Rise    | Suspense | Laugh   | Groove  | Groove2 | Groove3 | Outro1  | Outro2  |

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
import SimpleTypes (Rail, SmplRail, leftRail)

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

shift' :: Int -> OneTwoThreeFour -> SmplRail -> SmplRail
shift' i b = shift $ toNumber ((i * 4) + asInt b)

shift'' :: Int -> SmplRail -> SmplRail
shift'' i = shift $ toNumber (i * 4)

shift :: Number -> SmplRail -> SmplRail
shift n' = go
  where
  n = n' * 60.0 / toNumber tempo
  go e = e
    { timing = e.timing + n
    }

earliest :: Array SmplRail -> Number
earliest = go top
  where
  go x = Array.uncons >>> case _ of
    Nothing -> x
    Just { head: e, tail } -> go (min x e.timing) tail

toRail :: (Rail -> Rail) -> SmplRail -> SmplRail
toRail f = go
  where
  go e = e
    { column = f e.column
    }

squeeze :: Number -> Number -> SmplRail -> SmplRail
squeeze n refn = go
  where
  go e = e
    { timing = (e.timing - refn) * n + refn
    }

-- these basics exploit the fact that only the first timing is used
-- in this version
-- the 1111 is overkill
basic1 :: Int -> Array SmplRail
basic1 i = basic1111 i leftRail

basic1' :: Int -> Array SmplRail
basic1' i = map (shift 0.5) $ basic1 i

basic1x' :: Int -> Array SmplRail
basic1x' i = map (shift 0.25) $ basic1 i

basic1'x :: Int -> Array SmplRail
basic1'x i = map (shift 0.75) $ basic1 i

basic2 :: Int -> Array SmplRail
basic2 i = basic2424 i leftRail

basic2' :: Int -> Array SmplRail
basic2' i = map (shift 0.5) $ basic2 i

basic2x' :: Int -> Array SmplRail
basic2x' i = map (shift 0.25) $ basic2 i

basic2'x :: Int -> Array SmplRail
basic2'x i = map (shift 0.75) $ basic2 i

basic3 :: Int -> Array SmplRail
basic3 i = basic3131 i leftRail

basic3' :: Int -> Array SmplRail
basic3' i = map (shift 0.5) $ basic3 i

basic4 :: Int -> Array SmplRail
basic4 i = basic4242 i leftRail

basic4' :: Int -> Array SmplRail
basic4' i = map (shift 0.5) $ basic4 i

basic1313 :: Int -> Rail -> Array SmplRail
basic1313 i = basic i One Three One Three

basic2424 :: Int -> Rail -> Array SmplRail
basic2424 i = basic i Two Four Two Four

basic3131 :: Int -> Rail -> Array SmplRail
basic3131 i = basic i Three One Three One

basic4242 :: Int -> Rail -> Array SmplRail
basic4242 i = basic i Four Two Four Two

basic1234 :: Int -> Rail -> Array SmplRail
basic1234 i = basic i One Two Three Four

basic1111 :: Int -> Rail -> Array SmplRail
basic1111 i = basic i One One One One

basic2341 :: Int -> Rail -> Array SmplRail
basic2341 i = basic i Two Three Four One

basic3412 :: Int -> Rail -> Array SmplRail
basic3412 i = basic i Three Four One Two

basic4123 :: Int -> Rail -> Array SmplRail
basic4123 i = basic i Four One Two Three

basic :: Int -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> Rail -> Array SmplRail
basic m1 b1 b2 b3 b4 = basic' m1 b1 m2 b2 m3 b3 m4 b4
  where
  m2 = if b2 <= b1 then m1 + 1 else m1
  m3 = if b3 <= b2 then m2 + 1 else m2
  m4 = if b4 <= b3 then m3 + 1 else m3

basic'' :: Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Int -> OneTwoThreeFour -> Number -> Rail -> Array SmplRail
basic'' m1 b1 s1 _m2 _b2 _s2 _m3 _b3 _s3 _m4 _b4 _s4 column = pure $
  { timing: mbsToTime m1 b1 s1
  , column
  }

basic' :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Rail -> Array SmplRail
basic' m1 b1 m2 b2 m3 b3 m4 b4 = basic'' m1 b1 0.0 m2 b2 0.0 m3 b3 0.0 m4 b4 0.0

squeezeFromHead :: Number -> Array SmplRail -> Array SmplRail
squeezeFromHead n l = map (squeeze n stt) l
  where
  stt = earliest l

-- | By DJ Monad
-- |
-- | | 3 - 11 | 11 - 19 | 19 - 27 | 27 - 29  | 29 - 31 | 31 - 39 | 39 - 47 | 47 - 55 | 55 - 63 | 63 - 71 |
-- | | Intro  | Repeat  | Rise    | Suspense | Laugh   | Groove  | Groove2 | Groove3 | Outro1  | Outro2  |

intro :: Array SmplRail
intro = join
  [ basic3 4
  , basic3 6
  , basic3 7
  , basic3 8
  , basic3 9
  , basic3 10
  ]

intro'repeat :: Array SmplRail
intro'repeat = join
  [ basic3 12
  , basic3 14
  , basic3 15
  , basic3 16
  , basic3 17
  , basic3 18
  ]

rise :: Array SmplRail
rise = join
  [ basic1 19
  , basic1 21
  , basic1 23
  , basic3 23
  , basic1 24
  , basic3 24
  , basic1 25
  , basic3 25
  , basic1 26
  , basic3 26
  ]

suspense :: Array SmplRail
suspense = join
  [ basic1 27
  ]

laugh :: Array SmplRail
laugh = join
  [ basic1 29
  ]

groove :: Array SmplRail
groove = join
  [ basic3 31
  , basic3 32
  , basic3 33
  , basic3 34
  , basic2 35
  , basic4 35
  , basic2 36
  , basic4 36
  , basic2 37
  , basic4 37
  , basic2 38
  , basic4 38
  ]

groove2 :: Array SmplRail
groove2 = map (shift'' 8) groove

groove3 :: Array SmplRail
groove3 = join
  [ basic1 47
  , basic1 48
  , basic1 49
  , basic3 49
  , basic1 50
  , basic3 50
  , basic1 51
  , basic1 52
  , basic1 53
  , basic3 53
  , basic1 54
  , basic3 54
  ]

outro1 :: Array SmplRail
outro1 = join [ basic1 55, basic1 57, basic1 59, basic1 61 ]

outro2 :: Array SmplRail
outro2 = join [basic1 63, basic1 65 ]

type Events = V MultipleErrors (Array SmplRail)

noDice :: forall res. Int -> String -> V MultipleErrors res
noDice i s = invalid $ pure (ErrorAtIndex i (ForeignError s))

validateEvents :: Array SmplRail -> Events
validateEvents = traverseWithIndex \_i be -> pure be

piece :: forall m. MonadThrow Error m => m (Array SmplRail)
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
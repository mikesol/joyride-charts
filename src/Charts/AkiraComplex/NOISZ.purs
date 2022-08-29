-- | # NOISZ
-- |
-- | By Akira Complex
-- |
-- | Global form
-- | 1-18  | 18-34       | 34-66 | 66-82  | 82-98        | 98-130 | 130-(146)-152
-- | intro | ice / chain | fast  | intro2 | ice / chain2 | fast2  | outro (like intro)
-- | ## m. 1-18
-- | 
-- | Intro
-- |
-- | Longs on appearances, the occasional red, very open feel
-- |
-- | ## m. 18-26
-- |
-- | First "ice on my writsts and chain on my neck."
-- | Indivudal points floating in.
-- |
-- | ## m. 26-34
-- |
-- | Builds up.
-- | Indivudal points floating in, means adding greens, in the mix in, biased right.
-- | Left will be red right at the end
-- |
-- | ## m. 34-38
-- |
-- | Low density, high energy.
-- | Give people the opportunity to jump.
-- |
-- | ## m. 38-42
-- |
-- | Sloowwww.
-- | But lots of stuff. Thick
-- |
-- |
-- | ## m. 42-46
-- |
-- | Low density, high energy.
-- | Give people the opportunity to jump. A bit more spread than m. 34.
-- |
-- | ## m. 46-50
-- |
-- | Sloowwww.
-- | Lots of stuff, leads people to the right.
-- |
-- | ## m. 50-54
-- |
-- | Low density, high energy.
-- | Trails left, settles far left.
-- |
-- | ## m. 54-58
-- |
-- | Sloowwww.
-- | Flanking tiles trending right immediately.
-- |                              ---
-- |                  ---
-- |         ---
-- | ---
-- |
-- | ## m. 58-62
-- |
-- | Low density, high energy.
-- | All over the map
-- |
-- | ## m. 62-66
-- |
-- | Sloowwww.
-- | Mega-reward for the front. Very broad & slow.
-- |
-- | ## m. 66-74
-- |
-- | Spacey.
-- | Lots of greens.
-- |
-- | ## m. 72-82
-- |
-- | Still spacey.
-- | But intersperse some red.
-- | 
-- | ## m. 82-90
-- |
-- | Second "ice on my writsts and chain on my neck."
-- | 1-per beat, débit constant
-- |
-- | ## m. 90-94
-- |
-- | Second "ice on my writsts and chain on my neck."
-- | Staggered motion, things catching up with other things
-- |
-- | ## m. 94-98
-- |
-- | Two staggers across the field
-- | First slow
-- | Second following fast
-- |
-- | ## m. 98-102
-- |
-- | Slow, allow jumps, but now with a green to distract
-- |
-- |
-- | ## m. 102-106
-- |
-- | Mega thick
-- |
-- | ## m. 106-110
-- |
-- | Isolated reds, very spread, push to front
-- |
-- | ## m. 110-114
-- |
-- | Huge reward for front, nothing for back
-- | Happens twice
-- |
-- | ## m. 114-118
-- |
-- | Isolated reds, very consolodated, push to back
-- |
-- | ## m. 118-122
-- |
-- | Huge reward for back, nothing for front
-- | Happens twice
-- | Add green to shake stuff up?
-- |
-- | ## m. 122-126
-- |
-- | Somewhat dense
-- |
-- | ## m. 126-130
-- |
-- | Mega dense in first 2m
-- | Whittles down to almost nothing (or sudden to nothing) in middle
-- |
-- | ## m. 130-146
-- |
-- | Greens with the occassional red
-- | 
-- | ## m. 146-out
-- | Only reds to 1 very slow green
module Charts.AkiraComplex.NOISZ where

import Prelude

import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Joyride.Types (Column(..), EventV0(..), Event_(..), Position(..), Track(..))

data OneTwoThreeFour = One | Two | Three | Four

derive instance Eq OneTwoThreeFour
derive instance Ord OneTwoThreeFour

asInt :: OneTwoThreeFour -> Int
asInt One = 1
asInt Two = 2
asInt Three = 3
asInt Four = 4

tempo :: Int
tempo = 170

mbToTime :: Int -> OneTwoThreeFour -> Number
mbToTime m b = toNumber ((((m - 1) * 4) + (asInt b - 1)) * 60) / (toNumber tempo)

long :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Column -> Number -> Event_
long startM startB endM endB column length = EventV0 $ LongEventV0
  { marker1Time: mbToTime startM startB
  , marker2Time: mbToTime endM endB
  , audioURL: Nothing
  , column
  , length
  , name: Nothing
  , version: mempty
  }

leap :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Column -> Position -> Event_
leap startM startB endM endB column position = EventV0 $ LeapEventV0
  { marker1Time: mbToTime startM startB
  , marker2Time: mbToTime endM endB
  , audioURL: Nothing
  , column
  , position
  , name: Nothing
  , version: mempty
  }

basic :: Int -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> OneTwoThreeFour -> Column -> Event_
basic m1 b1 b2 b3 b4 = basic' m1 b1 m2 b2 m3 b3 m4 b4
  where
  m2 = if b2 <= b1 then m1 + 1 else m1
  m3 = if b3 <= b2 then m2 + 1 else m2
  m4 = if b4 <= b3 then m3 + 1 else m3

basic' :: Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Int -> OneTwoThreeFour -> Column -> Event_
basic' m1 b1 m2 b2 m3 b3 m4 b4 column = EventV0 $ BasicEventV0
  { marker1Time: mbToTime m1 b1
  , marker2Time: mbToTime m2 b2
  , marker3Time: mbToTime m3 b3
  , marker4Time: mbToTime m4 b4
  , marker1AudioURL: Nothing
  , marker2AudioURL: Nothing
  , marker3AudioURL: Nothing
  , marker4AudioURL: Nothing
  , column
  , name: Nothing
  , version: mempty
  }

intro :: Array Event_
intro =
  [ long 2 One 5 One C7 4.0
  , long 6 One 9 One C9 4.0
  , long 10 One 12 One C6 3.0
  , leap 11 One 13 One C8 Position1
  , long 12 One 12 One C10 3.0
  , leap 13 One 15 One C8 Position2
  , long 14 One 17 One C9 4.0
  , long 16 One 19 One C7 2.0
  , leap 16 Three 17 Three C8 Position1
  , long 17 One 19 One C8 2.0
  , leap 17 Three 18 Three C8 Position2
  ]

piece :: { track :: Track, events :: Array Event_ }
piece =
  { track: TrackV0
      { version: mempty
      , url: "https://cdn.filestackcontent.com/kG1ZasfRPSvsRd2QAMux"
      , title: Just "NOISZ"
      , private: true
      , owner: "OKA4OPZguFZOv9p58TBbokciIlq2"
      }
  , events: intro
  }
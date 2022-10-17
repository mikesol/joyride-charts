module Main where

import Prelude

-- import Charts.AkiraComplex.NOISZ as NOISZ
-- import Charts.Combatplayer.Speedy as Speedy
-- import Charts.Jun.Imp as Imp
-- import Charts.Testing.Stress as Stress
-- import Charts.Testing.Simple as Simple
import Charts.DJMonad.MysteryMansionMadnessRedo as MysteryMansionMadnessRedo
import Effect (Effect)
import Effect.Console (log)
import Simple.JSON as JSON

main :: Effect Unit
main = do
  MysteryMansionMadnessRedo.piece >>= log <<< JSON.writeJSON 

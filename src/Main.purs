module Main where

import Prelude

import Charts.AkiraComplex.NOISZ as NOISZ
import Charts.Testing.Stress as Stress
import Charts.Testing.Simple as Simple
import Effect (Effect)
import Effect.Console (log)
import Simple.JSON as JSON

main :: Effect Unit
main = do
  Simple.piece >>= log <<< JSON.writeJSON 

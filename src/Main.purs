module Main where

import Prelude

import Charts.AkiraComplex.NOISZ as NOISZ
import Effect (Effect)
import Effect.Console (log)
import Simple.JSON as JSON

main :: Effect Unit
main = do
  NOISZ.piece >>= log <<< JSON.writeJSON 

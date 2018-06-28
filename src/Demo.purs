module VegaLiteDemo where

import VegaLitePrelude
import Control.Monad.Eff (Eff)
import VegaLite

bg :: View
bg = Simple $ setData d $ viewPanel Bar encs
  where
  d = UrlData {url: "https://vega.github.io/vega-lite/data/seattle-weather.csv", format: Nothing}
  encs = 
    markX (posTimeUnit Month $ posField "date"$ position Ordinal) $
    markY (posAggregate Mean $ posField "precipitation" $ position Quantitative) $
    encoding

main :: Eff () Unit
main = pure $ embedView "#vis" bg
  


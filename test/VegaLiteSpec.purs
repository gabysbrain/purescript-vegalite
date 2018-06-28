module Test.VegaLiteSpec where

import VegaLitePrelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Test.SchemaValidate.Validator (validate)
import Test.QuickCheck (Result, withHelp)
import Test.Spec (Spec, pending, pending', describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut (jsonParser, encodeJson)

import VegaLite

type Effects e = QCRunnerEffects(fs::FS, exception::EXCEPTION | e)

bg :: View
bg = Simple $ setData d $ viewPanel Bar encs
  where
  d = UrlData {url: "data/seattle-weather.csv", format: Nothing}
  encs = 
    markX (posTimeUnit Month $ posField "date"$ position Ordinal) $
    markY (posAggregate Mean $ posField "precipitation" $ position Quantitative) $
    encoding

bgWAvg :: View
bgWAvg = Layered {data: Just d, layers: layers}
  where
  d = UrlData {url: "data/seattle-weather.csv", format: Nothing}
  barEncs = 
    markX (posTimeUnit Month $ posField "date"$ position Ordinal) $
    markY (posAggregate Mean $ posField "precipitation" $ position Quantitative) $
    encoding
  avgEncs = 
    markY     (posAggregate Mean $ posField "precipitation" $ position Quantitative) $
    markSize  (markPropValue $ intValue 3) $
    markColor (markPropValue $ strValue "firebrick") $
    encoding
  barLayer = viewPanel Bar barEncs
  avgLayer = viewPanel Rule avgEncs
  layers = nea barLayer [avgLayer]

bgWBrush :: View
bgWBrush = Layered {data: Just d, layers: layers}
  where
  d = UrlData {url: "data/seattle-weather.csv", format: Nothing}
  barEncs = 
    markX (posTimeUnit Month $ posField "date" $ position Ordinal) $
    markY (posAggregate Mean $ posField "precipitation" $ position Quantitative) $
    markOpacity (condMarkPropValue [brushSelectionValue 1.0] (numValue 0.7)) $
    encoding
  avgEncs = 
    markY     (posAggregate Mean $ posField "precipitation" $ position Quantitative) $
    markSize  (markPropValue $ intValue 3) $
    markColor (markPropValue $ strValue "firebrick") $
    encoding
  sel = intervalSelection [XChannel]
  barLayer = setSelection sel $ viewPanel Bar barEncs
  avgLayer = setTransforms [brushFilterTransform] $ viewPanel Rule avgEncs
  layers = nea barLayer [avgLayer]

loadVlSchema :: forall e. Aff (fs::FS,exception::EXCEPTION | e) Json
loadVlSchema = do
  txt <- liftEff $ readTextFile UTF8 "test/vega-lite-v2.spec.json"
  schema <- either (liftEff <<< throw) pure $ jsonParser txt
  pure schema

validateSchema :: Json -> View -> Result
validateSchema s v = 
  withHelp (validate s $ encodeJson v) 
           ("validation failed for " <> show (encodeJson v))

compareToFile :: forall r. String -> View -> Aff (Effects r) Unit
compareToFile fn v = do
  let path = "test/cases/" <> fn
  txt <- liftEff $ readTextFile UTF8 path
  expected <- either (liftEff <<< throw) pure $ jsonParser txt
  encodeJson v `shouldEqual` expected

spec :: forall r. Spec (Effects r) Unit
spec = describe "VegaLite test cases" do
  describe "Generating full specifications" do
    it "bargraph spec" do
      compareToFile "bargraph.json" bg
    it "bargraph w/ average spec" do
      compareToFile "bargraph2.json" bgWAvg
    it "bargraph w/ brush" do
      compareToFile "bargraph3.json" bgWBrush
  {--describe "Any possible specification" do--}
    {--it "should validate against vega-lite's schema" do--}
      {--schema <- loadVlSchema--}
      {--quickCheck \view -> validateSchema schema view--}


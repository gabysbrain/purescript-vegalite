module VegaLite where

import MyPrelude
import Control.Monad.Gen ( class MonadGen
                         , elements, choose, oneOf, frequency, unfoldable
                         , chooseBool, chooseFloat, chooseInt )
import Control.Monad.Gen.Common (genNonEmpty, genMaybe, genTuple, genEither)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Array as A
import Data.NonEmpty (NonEmpty, (:|))
import Data.Newtype (over)
import Data.Argonaut (Json, class EncodeJson, encodeJson, 
                      jsonEmptyObject, (:=), (~>))
import Test.QuickCheck (class Arbitrary, arbitrary)

-- json version of the vega-lite specification

-- FIXME: this is so wrong
genString :: forall m. MonadGen m => m String
genString = pure "test"

genArray :: forall m a. MonadRec m => MonadGen m => m a -> m (Array a)
genArray = unfoldable

genNumber :: forall m. MonadGen m => m Number
genNumber = chooseFloat 0.0 1.0

genInt :: forall m. MonadGen m => m Int
genInt = chooseInt 1 100

newtype NonemptyArray a = NonemptyArray (NonEmpty Array a)
derive instance newtypeNonemptyArray :: Newtype (NonemptyArray a) _

instance encodeNonemptyArray :: (EncodeJson a) => EncodeJson (NonemptyArray a) where
  encodeJson (NonemptyArray xs) = encodeJson $ A.fromFoldable xs

genNonemptyArray :: forall m a. MonadRec m => MonadGen m => m a -> m (NonemptyArray a)
genNonemptyArray gen = wrap <$> (genNonEmpty gen)

maybeRecord :: forall a. EncodeJson a => String -> Maybe a -> Json -> Json
maybeRecord _ Nothing json = json
maybeRecord k (Just v) json = k := v ~> json

arrayRecord :: forall a. EncodeJson a => String -> Array a -> Json -> Json
arrayRecord k arr json = if A.null arr
                            then json
                            else k := arr ~> json

-- TODO: not sure this is needed
data Value = N Number
           | D String -- Dates!
           | S String

instance encodeValue :: EncodeJson Value where
  encodeJson (N n) = encodeJson n
  encodeJson (D d) = encodeJson d
  encodeJson (S s) = encodeJson s

genValue :: forall m. MonadGen m => m Value
genValue = oneOf ((N <$> genNumber) :| [D <$> genString, S <$> genString])

data View = Simple ViewPanel

genView :: forall m. MonadGen m => m MarkType -> m Encoding -> m View
genView mt e = Simple <$> genViewPanel mt e

instance encodeView :: EncodeJson View where
  encodeJson (Simple p) = encodeJson p

instance arbView :: Arbitrary View where
  arbitrary = genView arbitrary arbitrary

newtype ViewPanel = ViewPanel
  { data :: Maybe Data
  , encoding :: Encoding
  , markType :: MarkType
  , selection :: Maybe Selection
  }
derive instance newtypeViewPanel :: Newtype ViewPanel _

instance encodeViewPanel :: EncodeJson ViewPanel where
  encodeJson (ViewPanel vp)
     = maybeRecord "data" vp.data
     $ maybeRecord "selection" vp.selection
     $ "encoding" := vp.encoding
    ~> "mark" := vp.markType
    ~> jsonEmptyObject

viewPanel :: MarkType -> Encoding -> ViewPanel
viewPanel m e = wrap {data:Nothing, encoding:e, markType:m, selection:Nothing}

genViewPanel :: forall m. MonadGen m => m MarkType -> m Encoding -> m ViewPanel
genViewPanel mt e = viewPanel <$> mt <*> e

setData :: Data -> ViewPanel -> ViewPanel
setData d = over ViewPanel (\vp -> vp {data=Just d})

data Data = UrlData { url :: String, format :: Maybe DataFormat }

data DataFormat = AutoFormat
                | JsonFormat
                | CsvFormat
                | TsvFormat

instance encodeDataFormat :: EncodeJson DataFormat where
  encodeJson AutoFormat = encodeJson "auto"
  encodeJson f
     = "type" := case f of
                     JsonFormat -> "json"
                     CsvFormat -> "csv"
                     TsvFormat -> "tsv"
                     _         -> "auto"
    ~> jsonEmptyObject

data MarkType = Area
              | Bar
              | Line
              | Point
              | Text
              | Tick
              | Rect
              | Rule
              | Circle
              | Square

genMarkType :: forall m. MonadGen m => m MarkType
genMarkType = elements (Area :| [Bar, Line, Point, Text, Tick, Rect, Rule, Circle, Square])

instance arbMarkType :: Arbitrary MarkType where
  arbitrary = genMarkType

-- TODO: maybe turn this into a set
newtype Encoding = Encoding
  { color :: Maybe CondMarkProp
  , column :: Maybe FacetFieldDef
  , row :: Maybe FacetFieldDef
  , detail :: Maybe FieldDef
  , opacity :: Maybe CondMarkProp
  , order :: Maybe SortSpec
  , shape :: Maybe CondMarkProp
  , size :: Maybe CondMarkProp
  , text :: Maybe CondTextDef
  , tooltip :: Maybe CondTextDef
  , x :: Maybe PositionDef
  , y :: Maybe PositionDef
  , x2 :: Maybe FieldDef
  , y2 :: Maybe FieldDef
  }
derive instance newtypeEncoding :: Newtype Encoding _

instance encodeEncoding :: EncodeJson Encoding where
  encodeJson (Encoding e) 
    = maybeRecord "color" e.color
    $ maybeRecord "column" e.column
    $ maybeRecord "row" e.row
    $ maybeRecord "detail" e.detail
    $ maybeRecord "opacity" e.opacity
    $ maybeRecord "order" e.order
    $ maybeRecord "shape" e.shape
    $ maybeRecord "size" e.size
    $ maybeRecord "text" e.text
    $ maybeRecord "tooltip" e.tooltip
    $ maybeRecord "x" e.x
    $ maybeRecord "y" e.y
    $ maybeRecord "x2" e.x2
    $ maybeRecord "y2" e.y2
    $ jsonEmptyObject

instance arbEncoding :: Arbitrary Encoding where
  arbitrary = genEncoding

genEncoding :: forall m. MonadRec m => MonadGen m => m Encoding
genEncoding = do
  c <- genMaybe genCondMarkProp
  col <- genMaybe genFacetFieldDef
  row <- genMaybe genFacetFieldDef
  d <- genMaybe genFieldDef
  alpha <- genMaybe genCondMarkProp
  ord <- genMaybe genSortSpec
  s <- genMaybe genCondMarkProp
  sz <- genMaybe genCondMarkProp
  txt <- genMaybe genCondTextDef
  tt <- genMaybe genCondTextDef
  x <- genMaybe genPositionDef
  y <- genMaybe genPositionDef
  x2 <- genMaybe genFieldDef
  y2 <- genMaybe genFieldDef
  pure $ wrap
    { color: c
    , column: col
    , row: row
    , detail: d
    , opacity: alpha
    , order: ord
    , shape: s
    , size: sz
    , text: txt
    , tooltip: tt
    , x: x
    , y: y
    , x2: x2
    , y2: y2
    }

encoding :: Encoding
encoding = wrap
  { color: Nothing
  , column: Nothing
  , row: Nothing
  , detail: Nothing
  , opacity: Nothing
  , order: Nothing
  , shape: Nothing
  , size: Nothing
  , text: Nothing
  , tooltip: Nothing
  , x: Nothing
  , y: Nothing
  , x2: Nothing
  , y2: Nothing
  }

--markColor :: CondMarkProp -> Encoding -> Encoding
--markColumn :: FacetFieldDef -> Encoding -> Encoding
--markRow :: FacetFieldDef -> Encoding -> Encoding
--markDetail :: FieldDef -> Encoding -> Encoding
--markOpacity :: CondMarkProp -> Encoding -> Encoding
--markOrder :: SortSpec -> Encoding -> Encoding
--markShape :: CondMarkProp -> Encoding -> Encoding
--markSize :: CondMarkProp -> Encoding -> Encoding
--markText :: CondTextDef -> Encoding -> Encoding
--markTooltip :: CondTextDef -> Encoding -> Encoding
markX :: PositionDef -> Encoding -> Encoding
markX d = over Encoding (\vp -> vp {x=Just d})
markY :: PositionDef -> Encoding -> Encoding
markY d = over Encoding (\vp -> vp {y=Just d})
--markX2 :: FieldDef -> Encoding -> Encoding
--markY2 :: FieldDef -> Encoding -> Encoding

newtype CondMarkProp = CondMarkProp
  { aggregate :: Maybe Aggregate
  , bin :: Maybe BinParams
  , condition :: Array Condition
  , field :: Maybe String -- TODO: convert to field spec for the repeat stuff
  , legend :: Maybe Legend
  , scale :: Maybe Scale
  , sort :: Maybe SortSpec
  , timeUnit :: Maybe TimeUnit
  , type :: DataType
  }
derive instance newtypeCondMarkProp :: Newtype CondMarkProp _

instance encodeCondMarkProp :: EncodeJson CondMarkProp where
  encodeJson (CondMarkProp md)
     = maybeRecord "aggregate" md.aggregate
     $ maybeRecord "bin" md.bin
     $ maybeRecord "field" md.field
     $ maybeRecord "legend" md.legend
     $ maybeRecord "scale" md.scale
     $ maybeRecord "sort" md.sort
     $ maybeRecord "timeUnit" md.timeUnit
     $ arrayRecord "condition" md.condition
     $ "type" := md.type
    ~> jsonEmptyObject

genCondMarkProp :: forall m. MonadRec m => MonadGen m => m CondMarkProp
genCondMarkProp = do
  a <- genMaybe genAggregate
  b <- genMaybe genBinParams
  c <- genArray genCondition
  f <- genMaybe genString
  l <- genMaybe genLegend
  s <- genMaybe genScale
  srt <- genMaybe genSortSpec
  tu <- genMaybe genTimeUnit
  t <- genDataType
  pure $ wrap
    { aggregate: a
    , bin: b
    , condition: c
    , field: f
    , legend: l
    , scale: s
    , sort: srt
    , timeUnit: tu
    , type: t
    }

newtype CondTextDef = CondTextDef
  { aggregate :: Maybe Aggregate
  , bin :: Maybe BinParams
  , condition :: Array Condition
  , field :: Maybe String -- TODO: convert to field spec for the repeat stuff
  , format :: Maybe String
  , timeUnit :: Maybe TimeUnit
  , type :: DataType
  }
derive instance newtypeCondTextDef :: Newtype CondTextDef _

genCondTextDef :: forall m. MonadRec m => MonadGen m => m CondTextDef
genCondTextDef = do
  a <- genMaybe genAggregate
  b <- genMaybe genBinParams
  c <- genArray genCondition
  f <- genMaybe genString
  fmt <- genMaybe genString
  tu <- genMaybe genTimeUnit
  t <- genDataType
  pure $ wrap
    { aggregate: a
    , bin: b
    , condition: c
    , field: f
    , format: fmt
    , timeUnit: tu
    , type: t
    }


instance encodeCondTextDef :: EncodeJson CondTextDef where
  encodeJson (CondTextDef d)
     = maybeRecord "aggregate" d.aggregate
     $ maybeRecord "bin" d.bin
     $ arrayRecord "condition" d.condition
     $ maybeRecord "field" d.field
     $ maybeRecord "format" d.format
     $ maybeRecord "timeUnit" d.timeUnit
     $ "type" := d.type
    ~> jsonEmptyObject

newtype PositionDef = PositionDef
  { aggregate :: Maybe Aggregate
  , axis :: Maybe Axis
  , bin :: Maybe BinParams
  , condition :: Array Condition
  , field :: Maybe String -- TODO: convert to field spec for the repeat stuff
  , scale :: Maybe Scale
  , sort :: Maybe SortSpec
  , stack :: Maybe StackOffset
  , timeUnit :: Maybe TimeUnit
  , type :: DataType
  }
derive instance newtypePositionDef :: Newtype PositionDef _

instance encodePositionDef :: EncodeJson PositionDef where
  encodeJson (PositionDef p)
    = maybeRecord "aggregate" p.aggregate
    $ maybeRecord "axis" p.axis
    $ maybeRecord "bin" p.bin
    $ arrayRecord "condition" p.condition
    $ maybeRecord "field" p.field
    $ maybeRecord "scale" p.scale
    $ maybeRecord "sort" p.sort
    $ maybeRecord "stack" p.stack
    $ maybeRecord "timeUnit" p.timeUnit
    $ "type" := p.type
   ~> jsonEmptyObject

genPositionDef :: forall m. MonadRec m => MonadGen m => m PositionDef
genPositionDef = do
  a <- genMaybe genAggregate
  axis <- genMaybe genAxis
  b <- genMaybe genBinParams
  c <- genArray genCondition
  f <- genMaybe genString
  s <- genMaybe genScale
  srt <- genMaybe genSortSpec
  stack <- genMaybe genStackOffset
  tu <- genMaybe genTimeUnit
  t <- genDataType
  pure $ wrap
    { aggregate: a
    , axis: axis
    , bin: b
    , condition: c
    , field: f
    , scale: s
    , sort: srt
    , stack: stack
    , timeUnit: tu
    , type: t
    }

position :: DataType -> PositionDef
position t = wrap
  { aggregate: Nothing
  , axis: Nothing
  , bin: Nothing
  , condition: []
  , field: Nothing
  , scale: Nothing
  , sort: Nothing
  , stack: Nothing
  , timeUnit: Nothing
  , type: t
  }

posAggregate :: Aggregate -> PositionDef -> PositionDef
posAggregate a = over PositionDef (\vp -> vp {aggregate=Just a})
posField :: String -> PositionDef -> PositionDef
posField fn = over PositionDef (\vp -> vp {field=Just fn})
posTimeUnit :: TimeUnit -> PositionDef -> PositionDef
posTimeUnit tu = over PositionDef (\vp -> vp {timeUnit=Just tu})

newtype FieldDef = FieldDef
  { aggregate :: Maybe Aggregate
  , bin :: Maybe BinParams
  , field :: Maybe String -- TODO: convert to field spec for the repeat stuff
  , timeUnit :: Maybe TimeUnit
  , type :: DataType
  }
derive instance newtypeFieldDef :: Newtype FieldDef _

instance encodeFieldDef :: EncodeJson FieldDef where
  encodeJson (FieldDef f)
     = maybeRecord "aggregate" f.aggregate
     $ maybeRecord "bin" f.bin
     $ maybeRecord "field" f.field
     $ maybeRecord "timeUnit" f.timeUnit
     $ "type" := f.type
    ~> jsonEmptyObject

genFieldDef :: forall m. MonadRec m => MonadGen m => m FieldDef
genFieldDef = do
  a <- genMaybe genAggregate
  b <- genMaybe genBinParams
  f <- genMaybe genString
  tu <- genMaybe genTimeUnit
  t <- genDataType
  pure $ wrap { aggregate: a, bin: b, field: f, timeUnit: tu, type: t }

newtype FacetFieldDef = FacetFieldDef
  { field :: FieldDef
  , header :: Maybe Header
  , sort :: Maybe SortOrder
  }
derive instance newtypeFacetFieldDef :: Newtype FacetFieldDef _

instance encodeFacetFieldDef :: EncodeJson FacetFieldDef where
  encodeJson (FacetFieldDef f)
     = maybeRecord "header" f.header
     $ maybeRecord "sort" f.sort
     $ "field" := f.field
    ~> jsonEmptyObject

genFacetFieldDef :: forall m. MonadRec m => MonadGen m => m FacetFieldDef
genFacetFieldDef = do
  f <- genFieldDef
  h <- genMaybe genHeader
  srt <- genMaybe genSortOrder
  pure $ wrap { field: f, header: h, sort: srt }

newtype Axis = Axis
  { domain :: Maybe Boolean
  , format :: Maybe String -- from d3's formatting
  , grid :: Maybe Boolean
  , labelAngle :: Maybe Number
  , labelBound :: Maybe (Either Boolean Number)
  , labelFlush :: Maybe (Either Boolean Number)
  , labelOverlap :: Maybe LabelOverlap
  , labelPadding :: Maybe Number
  , labels :: Maybe Boolean
  , maxExtent :: Maybe Number
  , minExtent :: Maybe Number
  , offset :: Maybe Number
  , orient :: Maybe AxisOrient
  , position :: Maybe Number
  , tickCount :: Maybe Int
  , tickSize :: Maybe Number
  , ticks :: Maybe Boolean
  , title :: Maybe String
  , titleMaxLength :: Maybe Number
  , titlePadding :: Maybe Number
  , values :: Array Number
  , zIndex :: Maybe Number
  }
derive instance newtypeAxis :: Newtype Axis _

instance encodeAxis :: EncodeJson Axis where
  encodeJson (Axis a)
     = maybeRecord "domain" a.domain
     $ maybeRecord "format" a.format
     $ maybeRecord "grid" a.grid
     $ maybeRecord "labelAngle" a.labelAngle
     $ maybeRecord "labelBound" a.labelBound
     $ maybeRecord "labelFlush" a.labelFlush
     $ maybeRecord "labelOverlap" a.labelOverlap
     $ maybeRecord "labelPadding" a.labelPadding
     $ maybeRecord "labels" a.labels
     $ maybeRecord "maxExtent" a.maxExtent
     $ maybeRecord "minExtent" a.minExtent
     $ maybeRecord "offset" a.offset
     $ maybeRecord "orient" a.orient
     $ maybeRecord "position" a.position
     $ maybeRecord "tickCount" a.tickCount
     $ maybeRecord "tickSize" a.tickSize
     $ maybeRecord "ticks" a.ticks
     $ maybeRecord "title" a.title
     $ maybeRecord "titleMaxLength" a.titleMaxLength
     $ maybeRecord "titlePadding" a.titlePadding
     $ arrayRecord "values" a.values
     $ maybeRecord "zIndex" a.zIndex
     $ jsonEmptyObject

genAxis :: forall m. MonadRec m => MonadGen m => m Axis
genAxis = do
  domain <- genMaybe chooseBool
  format <- genMaybe genString -- from d3's formatting
  grid <- genMaybe chooseBool
  labelAngle <- genMaybe genNumber
  labelBound <- genMaybe (genEither chooseBool genNumber)
  labelFlush <- genMaybe (genEither chooseBool genNumber)
  labelOverlap <- genMaybe genLabelOverlap
  labelPadding <- genMaybe genNumber
  labels <- genMaybe chooseBool
  maxExtent <- genMaybe genNumber
  minExtent <- genMaybe genNumber
  offset <- genMaybe genNumber
  orient <- genMaybe genAxisOrient
  pos <- genMaybe genNumber
  tickCount <- genMaybe genInt
  tickSize <- genMaybe genNumber
  ticks <- genMaybe chooseBool
  title <- genMaybe genString
  titleMaxLength <- genMaybe genNumber
  titlePadding <- genMaybe genNumber
  values <- genArray genNumber
  zIndex <- genMaybe genNumber
  pure $ wrap
    { domain: domain
    , format: format
    , grid: grid
    , labelAngle: labelAngle
    , labelBound: labelBound
    , labelFlush: labelFlush
    , labelOverlap: labelOverlap
    , labelPadding: labelPadding
    , labels: labels
    , maxExtent: maxExtent
    , minExtent: minExtent
    , offset: offset
    , orient: orient
    , position: pos
    , tickCount: tickCount
    , tickSize: tickSize
    , ticks: ticks
    , title: title
    , titleMaxLength: titleMaxLength
    , titlePadding: titlePadding
    , values: values
    , zIndex: zIndex
    }

data LabelOverlap = AllowOverlap
                  | Parity
                  | Greedy

instance encodeLabelOverlap :: EncodeJson LabelOverlap where
  encodeJson AllowOverlap = encodeJson false
  encodeJson Parity = encodeJson "parity"
  encodeJson Greedy = encodeJson "greedy"

genLabelOverlap :: forall m. MonadGen m => m LabelOverlap
genLabelOverlap = elements (AllowOverlap :| [Parity, Greedy])

data AxisOrient = Top | Bottom | Left | Right

instance encodeAxisOrient :: EncodeJson AxisOrient where
  encodeJson o = encodeJson $ case o of
    Top -> "top"
    Bottom -> "bottom"
    Left -> "left"
    Right -> "right"

genAxisOrient :: forall m. MonadGen m => m AxisOrient
genAxisOrient = elements (Top :| [Bottom, Left, Right])

newtype Header = Header
  { format :: Maybe String -- from d3's format strings
  , labelAngle :: Maybe Number
  , title :: Maybe String
  }
derive instance newtypeHeader :: Newtype Header _

instance encodeHeader :: EncodeJson Header where
  encodeJson (Header h)
     = maybeRecord "format" h.format
     $ maybeRecord "labelAngle" h.labelAngle
     $ maybeRecord "title" h.title
     $ jsonEmptyObject

genHeader :: forall m. MonadGen m => m Header
genHeader = do
  fmt <- genMaybe genString
  la <- genMaybe genNumber
  t <- genMaybe genString
  pure $ wrap { format: fmt, labelAngle: la, title: t }

newtype Condition = Condition
  { aggregate :: Maybe Aggregate
  , bin :: Maybe BinParams
  , field :: Maybe String -- TODO: Convert to field spec
  , legend :: Maybe Legend
  , scale :: Maybe Scale
  , selection :: SelectionOperand
  , sort :: Maybe SortSpec
  , timeUnit :: Maybe TimeUnit
  , type :: DataType
  }
derive instance newtypeCondition :: Newtype Condition _

instance encodeCondition :: EncodeJson Condition where
  encodeJson (Condition c)
     = maybeRecord "aggregate" c.aggregate
     $ maybeRecord "bin" c.bin
     $ maybeRecord "field" c.field
     $ maybeRecord "legend" c.legend
     $ maybeRecord "scale" c.scale
     $ maybeRecord "sort" c.sort
     $ maybeRecord "timeUnit" c.timeUnit
     $ "selection" := c.selection
    ~> "type" := c.type
    ~> jsonEmptyObject

genCondition :: forall m. MonadRec m => MonadGen m => m Condition
genCondition = do
  a <- genMaybe genAggregate
  b <- genMaybe genBinParams
  f <- genMaybe genString
  l <- genMaybe genLegend
  s <- genMaybe genScale
  sel <- genSelectionOperand unit
  srt <- genMaybe genSortSpec
  tu <- genMaybe genTimeUnit
  t <- genDataType
  pure $ wrap
    { aggregate: a
    , bin: b
    , field: f
    , legend: l
    , scale: s
    , selection: sel
    , sort: srt
    , timeUnit: tu
    , type: t
    }

newtype Legend = Legend
  { entryPadding :: Maybe Number
  , format :: Maybe String -- from d3's format string
  , offset :: Maybe Number
  , orient :: Maybe LegendOrient
  , padding :: Maybe Number
  , tickCount :: Maybe Int
  , title :: Maybe String
  , type :: Maybe LegendType
  , values :: Array Value
  , zIndex :: Maybe Int
  }
derive instance newtypeLegend :: Newtype Legend _

instance encodeLegend :: EncodeJson Legend where
  encodeJson (Legend l)
     = maybeRecord "entryPadding" l.entryPadding
     $ maybeRecord "format" l.format
     $ maybeRecord "offset" l.offset
     $ maybeRecord "orient" l.orient
     $ maybeRecord "padding" l.padding
     $ maybeRecord "tickCount" l.tickCount
     $ maybeRecord "title" l.title
     $ maybeRecord "type" l.type
     $ arrayRecord "values" l.values
     $ maybeRecord "zIndex" l.zIndex
     $ jsonEmptyObject

genLegend :: forall m. MonadRec m => MonadGen m => m Legend
genLegend = do
  ep <- genMaybe genNumber
  f <- genMaybe genString
  o <- genMaybe genNumber
  or <- genMaybe genLegendOrient
  p <- genMaybe genNumber
  tc <- genMaybe genInt
  tt <- genMaybe genString
  t <- genMaybe genLegendType
  v <- genArray genValue
  z <- genMaybe genInt
  pure $ wrap
    { entryPadding: ep
    , format: f
    , offset: o
    , orient: or
    , padding: p
    , tickCount: tc
    , title: tt
    , type: t
    , values: v
    , zIndex: z
    }


data LegendType = Symbol | Gradient

genLegendType :: forall m. MonadGen m => m LegendType
genLegendType = choose (pure Symbol) (pure Gradient)

instance encodeLegendType :: EncodeJson LegendType where
  encodeJson t = encodeJson $ case t of
    Symbol -> "symbol"
    Gradient -> "gradient"

data StackOffset = NoStack | CenterStack | NormalizeStack

instance encodeStackOffset :: EncodeJson StackOffset where
  encodeJson so = encodeJson $ case so of
    NoStack -> "zero"
    CenterStack -> "center"
    NormalizeStack -> "normalize"

genStackOffset :: forall m. MonadGen m => m StackOffset
genStackOffset = elements (NoStack :| [CenterStack, NormalizeStack])

data LegendOrient = LeftLegend
                  | RightLegend
                  | TopLeft
                  | TopRight
                  | BottomLeft
                  | BottomRight
                  | None 

instance encodeLegendOrient :: EncodeJson LegendOrient where
  encodeJson lo = encodeJson $ case lo of
    LeftLegend -> "left-legend"
    RightLegend -> "right-legend"
    TopLeft -> "top-left"
    TopRight -> "top-right"
    BottomLeft -> "bottom-left"
    BottomRight -> "bottom-right"
    None  -> "none"

genLegendOrient :: forall m. MonadGen m => m LegendOrient
genLegendOrient = elements (LeftLegend :| [RightLegend, TopLeft, TopRight, BottomLeft, BottomRight, None])

newtype Scale = Scale
  { base :: Maybe Number
  , clamp :: Maybe Boolean
  , domain :: NonemptyArray Value
  , range :: NonemptyArray Value
  , exponent :: Maybe Number
  , interpolate :: Maybe Interpolate
  , nice :: Maybe Boolean
  , padding :: Maybe Number
  , paddingInner :: Maybe Number
  , paddingOuter :: Maybe Number
  , rangeStep :: Maybe Number
  , round :: Maybe Boolean
  , scheme :: Maybe String
  , type :: Maybe ScaleType
  , zero :: Maybe Boolean
  }

derive instance newtypeScale :: Newtype Scale _

instance encodeScale :: EncodeJson Scale where
  encodeJson (Scale s)
     = maybeRecord "base" s.base
     $ maybeRecord "clamp" s.clamp
     $ maybeRecord "exponent" s.exponent
     $ maybeRecord "interpolate" s.interpolate
     $ maybeRecord "nice" s.nice
     $ maybeRecord "padding" s.padding
     $ maybeRecord "paddingInner" s.paddingInner
     $ maybeRecord "paddingOuter" s.paddingOuter
     $ maybeRecord "rangeStep" s.rangeStep
     $ maybeRecord "round" s.round
     $ maybeRecord "scheme" s.scheme
     $ maybeRecord "type" s.type
     $ maybeRecord "zero" s.zero
     $ "domain" := s.domain
    ~> "range" := s.range
    ~> jsonEmptyObject

genScale :: forall m. MonadRec m => MonadGen m => m Scale
genScale = do
  b <- genMaybe genNumber
  c <- genMaybe chooseBool
  d <- genNonemptyArray genValue
  r <- genNonemptyArray genValue
  exp <- genMaybe genNumber
  i <- genMaybe genInterpolate
  n <- genMaybe chooseBool
  p <- genMaybe genNumber
  pi <- genMaybe genNumber
  po <- genMaybe genNumber
  rs <- genMaybe genNumber
  rnd <- genMaybe chooseBool
  s <- genMaybe genString
  t <- genMaybe genScaleType
  z <- genMaybe chooseBool
  pure $ wrap
    { base: b
    , clamp: c
    , domain: d
    , range: r
    , exponent: exp
    , interpolate: i
    , nice: n
    , padding: p
    , paddingInner: pi
    , paddingOuter: po
    , rangeStep: rs
    , round: rnd
    , scheme: s
    , type: t
    , zero: z
    }

data SortOrder = Ascending | Descending

genSortOrder :: forall m. MonadGen m => m SortOrder
genSortOrder = choose (pure Ascending) (pure Descending)

instance encodeSortOrder :: EncodeJson SortOrder where
  encodeJson o = encodeJson $ case o of
    Ascending  -> "ascending"
    Descending -> "descending"

data DataType = Quantitative
              | Ordinal
              | Temporal
              | Nominal

instance encodeDataType :: EncodeJson DataType where
  encodeJson t = encodeJson $ case t of
    Quantitative -> "quantitative"
    Ordinal -> "ordinal"
    Temporal -> "temporal"
    Nominal -> "nominal"

genDataType :: forall m. MonadGen m => m DataType
genDataType = elements (Quantitative :| [Ordinal, Temporal, Nominal])

data Aggregate = Argmax
               | Argmin
               | Average
               | Count
               | Distinct
               | Max
               | Mean
               | Median
               | Min
               | Missing
               | Q1
               | Q3
               | Ci0
               | Ci1
               | Stdev
               | Stdevp
               | Sum
               | Valid
               | Values
               | Variance
               | Variancep 

instance encodeAggregate :: EncodeJson Aggregate where
  encodeJson a = encodeJson $ case a of
    Argmax -> "argmax"
    Argmin -> "argmin"
    Average -> "average"
    Count -> "count"
    Distinct -> "distinct"
    Max -> "max"
    Mean -> "mean"
    Median -> "median"
    Min -> "min"
    Missing -> "missing"
    Q1 -> "q1"
    Q3 -> "q3"
    Ci0 -> "ci0"
    Ci1 -> "ci1"
    Stdev -> "stdev"
    Stdevp -> "stdevp"
    Sum -> "sum"
    Valid -> "valid"
    Values -> "values"
    Variance -> "variance"
    Variancep  -> "variancep"

genAggregate :: forall m. MonadGen m => m Aggregate
genAggregate = elements (Argmax :| [Argmin, Average, Count, Distinct, Max, Mean, Median, Min, Missing, Q1, Q3, Ci0, Ci1, Stdev, Stdevp, Sum, Valid, Values, Variance, Variancep])

data Interpolate = Linear
                 | LinearClosed
                 | Step
                 | StepBefore
                 | StepAfter
                 | Basis
                 | BasisOpen
                 | BasisClosed
                 | Cardinal
                 | CardinalOpen
                 | CardinalClosed
                 | Bundle
                 | Monotone

instance encodeInterpolate :: EncodeJson Interpolate where
  encodeJson i = encodeJson $ case i of
    Linear -> "linear"
    LinearClosed -> "linearClosed"
    Step -> "step"
    StepBefore -> "stepBefore"
    StepAfter -> "stepAfter"
    Basis -> "basis"
    BasisOpen -> "basisOpen"
    BasisClosed -> "basisClosed"
    Cardinal -> "cardinal"
    CardinalOpen -> "cardinalOpen"
    CardinalClosed -> "cardinalClosed"
    Bundle -> "bundle"
    Monotone -> "monotone"

genInterpolate :: forall m. MonadGen m => m Interpolate
genInterpolate = elements (Linear :|
  [ LinearClosed
  , Step
  , StepBefore
  , StepAfter
  , Basis
  , BasisOpen
  , BasisClosed
  , Cardinal
  , CardinalOpen
  , CardinalClosed
  , Bundle
  , Monotone 
  ])

data ScaleType = LinearScale
               | BinLinearScale
               | LogScale
               | PowScale
               | SqrtScale
               | TimeScale
               | UtcScale
               | SequentialScale
               | OrdinalScale
               | BinOrdinalScale
               | PointScale
               | BandScale

instance encodeScaleType :: EncodeJson ScaleType where
  encodeJson st = encodeJson $ case st of
    LinearScale -> "linearScale"
    BinLinearScale -> "binLinearScale"
    LogScale -> "logScale"
    PowScale -> "powScale"
    SqrtScale -> "sqrtScale"
    TimeScale -> "timeScale"
    UtcScale -> "utcScale"
    SequentialScale -> "sequentialScale"
    OrdinalScale -> "ordinalScale"
    BinOrdinalScale -> "binOrdinalScale"
    PointScale -> "pointScale"
    BandScale -> "bandScale"

genScaleType :: forall m. MonadGen m => m ScaleType
genScaleType = elements (LinearScale :|
  [ BinLinearScale
  , LogScale
  , PowScale
  , SqrtScale
  , TimeScale
  , UtcScale
  , SequentialScale
  , OrdinalScale
  , BinOrdinalScale
  , PointScale
  , BandScale
  ])

data BinParams =
    B Boolean
  | P { base :: Maybe Number
      , divide :: Maybe (NonemptyArray Number)
      , extent :: Maybe (Tuple Number Number)
      , maxBins :: Maybe Number
      , minStep :: Maybe Number
      , nice :: Maybe Boolean
      , step :: Maybe Number
      , steps :: Maybe (NonemptyArray Number)
      }

genBinParams :: forall m. MonadRec m => MonadGen m => m BinParams
genBinParams = choose (B <$> chooseBool) (P <$> genBinParams')

genBinParams' = do
  b <- genMaybe genNumber
  d <- genMaybe (genNonemptyArray genNumber)
  e <- genMaybe (genTuple genNumber genNumber)
  mb <- genMaybe genNumber
  ms <- genMaybe genNumber
  n <- genMaybe chooseBool
  s <- genMaybe genNumber
  st <- genMaybe (genNonemptyArray genNumber)
  pure 
    { base: b
    , divide: d
    , extent: e
    , maxBins: mb
    , minStep: ms
    , nice: n
    , step: s
    , steps: st
    }

instance encodeBinParams :: EncodeJson BinParams where
  encodeJson (B b) = encodeJson b
  encodeJson (P p)
     = maybeRecord "base" p.base
     $ maybeRecord "divide" p.divide
     $ maybeRecord "extent" p.extent
     $ maybeRecord "maxBins" p.maxBins
     $ maybeRecord "minStep" p.minStep
     $ maybeRecord "nice" p.nice
     $ maybeRecord "step" p.step
     $ maybeRecord "steps" p.steps
     $ jsonEmptyObject

data SelectionOperand = NotOper SelectionOperand
                      | AndOper SelectionOperand
                      | OrOper SelectionOperand
                      | Oper String

instance encodeSelectionOperand :: EncodeJson SelectionOperand where
  encodeJson (Oper s)    = encodeJson s
  encodeJson (NotOper o) = "not" := o ~> jsonEmptyObject
  encodeJson (AndOper o) = "and" := o ~> jsonEmptyObject
  encodeJson (OrOper o)  = "or" := o ~> jsonEmptyObject

-- need an arg to handle the recursive stuff
genSelectionOperand :: forall m. MonadGen m => Unit -> m SelectionOperand
genSelectionOperand _ = Oper <$> genString
{--genSelectionOperand x = frequency --}
  {--( (Tuple 0.05 (NotOper <$> genSelectionOperand x)) :|--}
  {--[ Tuple 0.05 (AndOper <$> genSelectionOperand x)--}
  {--, Tuple 0.05 (OrOper <$> genSelectionOperand x)--}
  {--, Tuple 0.85 (Oper <$> genString)--}
  {--])--}

data SortSpec = OrderSort SortOrder
              | FieldSort SortField
              | NoSort

instance encodeSortSpec :: EncodeJson SortSpec where
  encodeJson (OrderSort s) = encodeJson s
  encodeJson (FieldSort f) = encodeJson f
  encodeJson NoSort = encodeJson unit

genSortSpec :: forall m. MonadGen m => m SortSpec
genSortSpec = oneOf $ ((OrderSort <$> genSortOrder) :| 
  [ FieldSort <$> genSortField, pure NoSort ])

newtype SortField = SortField
  { field :: Maybe String -- FIXME: add repeat spec
  , op :: Aggregate
  , order :: Maybe SortOrder
  }
derive instance newtypeSortField :: Newtype SortField _

instance encodeSortField :: EncodeJson SortField where
  encodeJson (SortField f)
     = maybeRecord "field" f.field
     $ maybeRecord "order" f.order
     $ "op" := f.op
    ~> jsonEmptyObject

genSortField :: forall m. MonadGen m => m SortField
genSortField = do
  f <- genMaybe genString
  op <- genAggregate
  ord <- genMaybe genSortOrder
  pure $ wrap
    { field: f
    , op: op
    , order: ord
    }

-- TODO: Fill in
data Selection = SingleSelection
               | MultiSelection
               | IntervalSelection

genSelection :: forall m. MonadGen m => m Selection
genSelection = elements (SingleSelection :| [MultiSelection, IntervalSelection])

data TimeUnit 
  -- Local multi-time units
  = Yearquarter
  | Yearquartermonth
  | Yearmonth
  | Yearmonthdate
  | Yearmonthdatehours
  | Yearmonthdatehoursminutes
  | Yearmonthdatehoursminutesseconds
  | Quartermonth
  | Monthdate
  | Hoursminutes
  | Hoursminutesseconds
  | Minutesseconds
  | Secondsmilliseconds
  -- Local single time units
  | Year
  | Quarter
  | Month
  | Day
  | Date
  | Hours
  | Minutes
  | Seconds
  | Milliseconds
  -- UTC multi-time units
  | Utcyearquarter
  | Utcyearquartermonth
  | Utcyearmonth
  | Utcyearmonthdate
  | Utcyearmonthdatehours
  | Utcyearmonthdatehoursminutes
  | Utcyearmonthdatehoursminutesseconds
  | Utcquartermonth
  | Utcmonthdate
  | Utchoursminutes
  | Utchoursminutesseconds
  | Utcminutesseconds
  | Utcsecondsmilliseconds
  -- UTC single time units
  | Utcyear
  | Utcquarter
  | Utcmonth
  | Utcday
  | Utcdate
  | Utchours
  | Utcminutes
  | Utcseconds
  | Utcmilliseconds

instance encodeTimeUnit :: EncodeJson TimeUnit where
  encodeJson tu = encodeJson $ case tu of
    Yearquarter -> "yearquarter"
    Yearquartermonth -> "yearquartermonth"
    Yearmonth -> "yearmonth"
    Yearmonthdate -> "yearmonthdate"
    Yearmonthdatehours -> "yearmonthdatehours"
    Yearmonthdatehoursminutes -> "yearmonthdatehoursminutes"
    Yearmonthdatehoursminutesseconds -> "yearmonthdatehoursminutesseconds"
    Quartermonth -> "quartermonth"
    Monthdate -> "monthdate"
    Hoursminutes -> "hoursminutes"
    Hoursminutesseconds -> "hoursminutesseconds"
    Minutesseconds -> "minutesseconds"
    Secondsmilliseconds -> "secondsmilliseconds"
    Year -> "year"
    Quarter -> "quarter"
    Month -> "month"
    Day -> "day"
    Date -> "date"
    Hours -> "hours"
    Minutes -> "minutes"
    Seconds -> "seconds"
    Milliseconds -> "milliseconds"
    Utcyearquarter -> "utcyearquarter"
    Utcyearquartermonth -> "utcyearquartermonth"
    Utcyearmonth -> "utcyearmonth"
    Utcyearmonthdate -> "utcyearmonthdate"
    Utcyearmonthdatehours -> "utcyearmonthdatehours"
    Utcyearmonthdatehoursminutes -> "utcyearmonthdatehoursminutes"
    Utcyearmonthdatehoursminutesseconds -> "utcyearmonthdatehoursminutesseconds"
    Utcquartermonth -> "utcquartermonth"
    Utcmonthdate -> "utcmonthdate"
    Utchoursminutes -> "utchoursminutes"
    Utchoursminutesseconds -> "utchoursminutesseconds"
    Utcminutesseconds -> "utcminutesseconds"
    Utcsecondsmilliseconds -> "utcsecondsmilliseconds"
    Utcyear -> "utcyear"
    Utcquarter -> "utcquarter"
    Utcmonth -> "utcmonth"
    Utcday -> "utcday"
    Utcdate -> "utcdate"
    Utchours -> "utchours"
    Utcminutes -> "utcminutes"
    Utcseconds -> "utcseconds"
    Utcmilliseconds -> "utcmilliseconds"

genTimeUnit :: forall m. MonadGen m => m TimeUnit
genTimeUnit = elements ( Yearquarter :|
  [ Yearquartermonth
  , Yearmonth
  , Yearmonthdate
  , Yearmonthdatehours
  , Yearmonthdatehoursminutes
  , Yearmonthdatehoursminutesseconds
  , Quartermonth
  , Monthdate
  , Hoursminutes
  , Hoursminutesseconds
  , Minutesseconds
  , Secondsmilliseconds
  , Year
  , Quarter
  , Month
  , Day
  , Date
  , Hours
  , Minutes
  , Seconds
  , Milliseconds
  , Utcyearquarter
  , Utcyearquartermonth
  , Utcyearmonth
  , Utcyearmonthdate
  , Utcyearmonthdatehours
  , Utcyearmonthdatehoursminutes
  , Utcyearmonthdatehoursminutesseconds
  , Utcquartermonth
  , Utcmonthdate
  , Utchoursminutes
  , Utchoursminutesseconds
  , Utcminutesseconds
  , Utcsecondsmilliseconds
  , Utcyear
  , Utcquarter
  , Utcmonth
  , Utcday
  , Utcdate
  , Utchours
  , Utcminutes
  , Utcseconds
  , Utcmilliseconds
  ])


instance encodeData :: EncodeJson Data where
  encodeJson (UrlData d)
     = "url" := d.url
    ~> "format" := d.format

instance encodeSelection :: EncodeJson Selection where
  encodeJson SingleSelection = jsonEmptyObject
  encodeJson MultiSelection = jsonEmptyObject
  encodeJson IntervalSelection = jsonEmptyObject

instance encodeMarkType :: EncodeJson MarkType where
  encodeJson mt = encodeJson $ case mt of
    Area   -> "area"
    Bar    -> "bar"
    Line   -> "line"
    Point  -> "point"
    Text   -> "text"
    Tick   -> "tick"
    Rect   -> "rect"
    Rule   -> "rule"
    Circle -> "circle"
    Square -> "square"


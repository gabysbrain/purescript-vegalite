module VegaLite.Grammar where

import Data.Newtype (class Newtype, wrap)
import Data.NonEmpty (NonEmpty)

type NonEmptyArray = NonEmpty Array

-- The vega-lite grammar implemented with types

-- Basic types
type Tuple = String -- FIXME: refine this
type Expression = String -- FIXME: refine this
type EventSelector = String -- FIXME: refine this
data Value = S String
           | N Number
           | B Boolean

data View = Simple ViewPanel
          | Layer   (NonEmptyArray ViewPanel) ViewResolve
          | HConcat (NonEmptyArray ViewPanel) ViewResolve
          | VConcat (NonEmptyArray ViewPanel) ViewResolve
          | Facet Channel Field Scale Axis View ViewResolve
          | Repeat Channel (NonEmptyArray Value) Scale Axis View ViewResolve

type ViewResolve = Array Resolution
newtype Resolution = Resolution {channel :: Channel, type :: ResType, fixme :: Boolean}
data ResType = ScaleRes | GuideRes

-- what we call a ViewPanel vega-lite calls a Unit but unit is a special 
-- word in purescript/haskell

newtype ViewPanel = ViewPanel 
  { data :: Data
  , transforms :: Transforms
  , markType :: MarkType
  , encodings :: Encodings
  , selections :: Selections
  }
derive instance newtypeViewPanel :: Newtype ViewPanel _

viewPanel :: Data -> Transforms -> MarkType -> Encodings -> Selections -> ViewPanel
viewPanel d t mt e s = wrap 
  { data: d
  , transforms: t
  , markType: mt
  , encodings: e
  , selections: s
  }
  

data Data = UrlData {url :: String, formatType :: FormatType}
data FormatType = JSON | CSV | TSV

jsonData :: String -> Data
jsonData u = UrlData {url: u, formatType: JSON}
csvData :: String -> Data
csvData u = UrlData {url: u, formatType: CSV}
tsvData :: String -> Data
tsvData u = UrlData {url: u, formatType: TSV}

type Transforms = Array Transform
data Transform = FilterNull Boolean
               | Calculate (NonEmptyArray Formula)
               | Filter Expression
               | FilterWith String

newtype Formula = Formula {name :: String, expr :: Expression}

data MarkType = Bar | Line | Area | Text | Rule | Dot | Tick

type Encodings = NonEmptyArray Encoding
newtype Encoding = Encoding { channel :: Channel, rule :: Rule }
derive instance newtypeEncoding :: Newtype Encoding _

encoding :: Channel -> Field -> Encoding
encoding c f = wrap {channel: c, rule: F f}

data Channel = X | Y | Color | Shape | Size | Label | Detail | Order | Path
type Channels = NonEmptyArray Channel
data Rule = F Field | IC (NonEmptyArray IfClause)

newtype IfClause = IfClause {selName :: String, field :: Field}

-- data fields

newtype Field = Field
  { name :: String
  , type :: DataType
  , function :: TransFunction
  , scale :: Scale
  , guide :: Guide
  }
derive instance newtypeField :: Newtype Field _

type Fields = NonEmptyArray Field

data DataType = Nominal | Ordinal | Quantitative | Temporal

data TransFunction = Identity | Bin | Mean | Average | Min | Max | Sum | Product

-- scale stuff

newtype Scale = Scale
  { type :: ScaleType
  , domain :: ScaleDomain
  , range :: ScaleRange
  , round :: Boolean
  , clamp :: Boolean
  , exponent :: Number
  , nice :: Boolean
  , zero :: Boolean
  }

type Scales = NonEmptyArray Scale

data ScaleType = LinearScale
               | OrdinalScale
               | LogScale
               | PowScale
               | SqrtScale
               | QuantileScale
               | QuantizeScale
               | ThresholdScale
               | TimeScale
newtype ScaleDomain = ScaleDomain (NonEmptyArray Value)
data ScaleRange = Str String | Lst (NonEmptyArray Value)

data Guide = A Axis | L Legend
newtype Axis = Axis
  { layer :: AxisLayer
  , offset :: AxisOffset
  , orient :: AxisOrient
  , grid :: AxisGrid
  , labels :: Labels
  , ticks :: Ticks
  , title :: Title
  }
newtype Legend = Legend
  { orient :: LegendOrient
  , title :: TitleText
  , format :: Format
  , shortTime :: Boolean
  , values :: NonEmptyArray Value
  }

data AxisLayer = Front | Back
type AxisOffset = Number
data AxisOrient = Top | Bottom | Left | Right
type AxisGrid = Boolean

data LegendOrient = LeftLegend | RightLegend
type Format = String

newtype Labels = Labels
  { show      :: Boolean
  , format    :: String
  , angle     :: Number
  , maxLength :: Number
  , shortTime :: Boolean
  }

newtype Ticks = Ticks
  { subdivide :: Number
  , count     :: Number
  , padding   :: Number
  , tickSize  :: Number
  , sizeMajor :: Number
  , sizeMinor :: Number
  , sizeEnd   :: Number
  }

newtype Title = Title
  { text           :: TitleText
  , offset         :: Number
  , maxLength      :: Number
  , characterWidth :: Number
  }
type TitleText = String

-- Interaction stuff

type Selections = Array Selection

newtype Selection = Selection
  { name :: String
  , type :: SelType
  , predicate :: Predicate
  , domainRange :: DomainRange
  , event :: Event
  , init :: SelInit
  , transforms :: SelTransforms
  , resolve :: SelResolve
  }

data SelType = Point | List | Interval
type Predicate = Expression
data DomainRange = Domain | Range
type Event = EventSelector
type Events = NonEmptyArray Event
data SelInit = SInit Scales | TInit Tuple | MInit (NonEmptyArray Tuple)
type SelTransforms = Array SelTransform
data SelTransform = Project Fields Channels
                  | Toggle Event
                  | Translate Events Number
                  | Zoom Event Number
                  | Nearest
data SelResolve = Single
                | Independent
                | Union
                | Intersect
                | UnionOthers
                | IntersectOthers


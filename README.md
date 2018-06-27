
# purescript-vegalite

A wrapper around the [vega-lite](https://vega.github.io/vega-lite/) 
visualization library.

## Installation

```
bower install purescript-vegalite
```

## Demo

There is a demo page illustrating how to use the module. You can view this
using pulp's server feature.

```
npm install
pulp demo
```

Then, there will be a demo page at http://127.0.0.1:1337. `src/Demo.purs`
contains the source for the demo app.

## Example

Here is the json file from the [front page](https://vega.github.io/vega-lite)
with a simple bar graph:

```
{
  "data": {"url": "https://vega.github.io/vega-lite/data/seattle-weather.csv"},
  "mark": "bar",
  "encoding": {
    "x": {
      "timeUnit": "month",
      "field": "date",
      "type": "ordinal"
    },
    "y": {
      "aggregate": "mean",
      "field": "precipitation",
      "type": "quantitative"
    }
  }
}
```

and here is the purescript equivalent:

```
bg :: View
bg = Simple $ setData d $ viewPanel Bar encs
  where
  d = UrlData {url: "https://vega.github.io/vega-lite/data/seattle-weather.csv", format: Nothing}
  encs = 
    markX (posTimeUnit Month $ posField "date"$ position Ordinal) $
    markY (posAggregate Mean $ posField "precipitation" $ position Quantitative) $
    encoding
```

## Development

Right now pulp is used for the build tool.  There seems to be a bug with the
canvas library or vega that prevents embedding these modules in the app.js
build.

```
pulp build
pulp test
```




var Ajv = require('ajv');

var ajv = new Ajv({verbose: true});
// vega-lite runs on draft-04 schema
//ajv.addMetaSchema(require('ajv/lib/refs/json-schema-draft-04.json'));

exports.validateImpl = function(schema, json) {
  var valid = ajv.validate(schema, json);
  if(!valid) {
    console.log('===================================');
    console.log(ajv.errors);
  }
  return valid;
};


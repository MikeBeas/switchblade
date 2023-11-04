const util = require('util');

module.exports.removeUndefined = (data) => {
  for (const key of Object.keys(data)) {
    if (data[key] === undefined) delete data[key];
  }

  return data;
}

module.exports.cleanString = (str) => {
  const trimmed = str?.trim();
  return !trimmed || trimmed === "" ? null : trimmed;
}

module.exports.prettyPrint = (value) => console.log('STORAGE', util.inspect(value, false, null, true))
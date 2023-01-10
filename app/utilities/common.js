module.exports.removeUndefined = (data) => {
  for (const key of Object.keys(data)) {
    if (data[key] === undefined) delete data[key];
  }

  return data;
}
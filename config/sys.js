const self = require('../package.json');
const dotenv = require('dotenv');
dotenv.config();

module.exports = {
  production: process.env.NODE_ENV === "production",
  version: self.version
}
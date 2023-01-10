const coreRoutes = require('./core');
const shortcutRoutes = require('./shortcuts');
const versionRoutes = require('./versions');
const userRoutes = require('./me');

module.exports = (app) => {
  coreRoutes(app)
  shortcutRoutes(app)
  versionRoutes(app),
  userRoutes(app)
}
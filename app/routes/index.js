const coreRoutes = require('./core');
const shortcutRoutes = require('./shortcuts');
const versionRoutes = require('./versions');
const userRoutes = require('./me');
const usersRoutes = require('./users');
const autocompleteRoutes = require('./autocomplete');

module.exports = (app) => {
  coreRoutes(app)
  shortcutRoutes(app)
  versionRoutes(app),
  userRoutes(app),
  usersRoutes(app),
  autocompleteRoutes(app)
}
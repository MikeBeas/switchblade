const auth = require('../middleware/auth');
const controller = require('../controllers/VersionController');
const { permissions } = require('../middleware/permissions');

module.exports = (app) => {
  app.get("/shortcuts/:shortcutId/version/:versionNumber", permissions.viewShortcutVersion, controller.getVersion);
  app.get("/shortcuts/:shortcutId/history", permissions.getVersionHistory, controller.getHistory);

  app.post("/shortcuts/:shortcutId/version", auth, permissions.createVersion, controller.createVersion);
  app.patch("/shortcuts/:shortcutId/version/:versionNumber", auth, permissions.modifyVersion, controller.modifyVersion);
}
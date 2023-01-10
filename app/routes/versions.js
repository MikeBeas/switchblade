const auth = require('../middleware/auth');
const controller = require('../controllers/VersionController');

module.exports = (app) => {
  app.get("/shortcuts/:shortcutId/version/:versionNumber", controller.getVersion);
  app.get("/shortcuts/:shortcutId/history", controller.getHistory);

  app.post("/shortcuts/:shortcutId/version", auth, controller.createVersion);
  app.patch("/shortcuts/:shortcutId/version/:versionNumber", auth, controller.modifyVersion);
}
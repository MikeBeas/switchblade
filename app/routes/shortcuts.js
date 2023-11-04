const auth = require('../middleware/auth');
const controller = require('../controllers/ShortcutController');
const { permissions } = require('../middleware/permissions');

module.exports = (app) => {
  app.get("/shortcuts", controller.getAllShortcuts);
  app.get("/shortcuts/:shortcutId", permissions.viewShortcut, controller.getShortcut);

  app.post("/shortcuts", auth, permissions.createShortcut, controller.createShortcut);
  app.patch("/shortcuts/:shortcutId", auth, permissions.modifyShortcut, controller.modifyShortcut);
}
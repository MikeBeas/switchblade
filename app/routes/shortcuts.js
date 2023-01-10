const auth = require('../middleware/auth');
const controller = require('../controllers/ShortcutController');

module.exports = (app) => {
  app.get("/shortcuts", controller.getAllShortcuts);
  app.get("/shortcuts/:shortcutId", controller.getShortcut);

  app.post("/shortcuts", auth, controller.createShortcut);
  app.patch("/shortcuts/:shortcutId", auth, controller.modifyShortcut);
}
const SystemController = require('../controllers/SystemController');
const UserController = require('../controllers/MeController');
const auth = require('../middleware/auth');

module.exports = (app) => {
  app.get("/", SystemController.getRoot);

  app.post("/login", UserController.login);
  app.get("/verify", auth, UserController.verify);

  // system setup -- one time use!
  app.post("/setup", SystemController.setup);

  // can be used for resetting passwords the hard way if you forget yours somehow
  app.post("/hash-password", SystemController.generatePasswordHash);
}
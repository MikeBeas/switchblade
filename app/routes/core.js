const sys = require('../../config/sys');
const security = require('../services/SecurityService');
const SystemController = require('../controllers/SystemController');
const UserController = require('../controllers/UserController');
const auth = require('../middleware/auth');

module.exports = (app) => {
  app.get("/", async (req, res) => res.send({
    api: {
      host: req.headers.host,
      production: sys.production,
      authenticated: await security.isAuthorized(req, res)
    },
    switchblade: {
      version: sys.version
    }
  }));

  app.post("/login", UserController.login);
  app.get("/verify", auth, UserController.verify);

  // system setup -- one time use!
  app.post("/setup", SystemController.setup);

  // can be used for resetting passwords the hard way if you forget yours somehow
  app.post("/hash-password", SystemController.generatePasswordHash);
}
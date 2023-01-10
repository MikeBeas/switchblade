const UserController = require('../controllers/UserController');
const auth = require('../middleware/auth');

module.exports = (app) => {
  app.get("/me", auth, UserController.getMyIdentity)
  app.patch("/me", auth, UserController.modifyMyIdentity)

  app.post("/me/mfa/setup", auth, UserController.beginMfaSetup)
  app.post("/me/mfa/complete", auth, UserController.completeMfaSetup)
  app.delete("/me/mfa", auth, UserController.deleteMfaConfiguration)
}
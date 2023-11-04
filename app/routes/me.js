const controller = require('../controllers/MeController');
const auth = require('../middleware/auth');

module.exports = (app) => {
  app.get("/me", auth, controller.getMyIdentity)
  app.patch("/me", auth, controller.modifyMyIdentity)

  app.post("/me/mfa/setup", auth, controller.beginMfaSetup)
  app.post("/me/mfa/complete", auth, controller.completeMfaSetup)
  app.delete("/me/mfa", auth, controller.deleteMfaConfiguration)
}
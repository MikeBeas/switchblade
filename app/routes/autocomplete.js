const controller = require('../controllers/UsersController');
const auth = require('../middleware/auth');

module.exports = (app) => {
  app.get("/autocomplete/users", auth, controller.autocomplete)
}
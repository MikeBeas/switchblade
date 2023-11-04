const auth = require('../middleware/auth');
const controller = require('../controllers/UsersController');
const { permissions } = require('../middleware/permissions');

module.exports = (app) => {
  app.get("/users", auth, permissions.getUsers, controller.getAllUsers);
  app.get("/users/:userId", auth, permissions.getUsers, controller.getUser);

  app.post("/users", auth, permissions.createUser, controller.createUser);
  app.patch("/users/:userId", auth, permissions.modifyUser, controller.modifyUser);
}
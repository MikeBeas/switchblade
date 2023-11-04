const UsersService = require('../services/UserService');
const { HTTP } = require('../constants');
const { getUserFromToken } = require('../services/SecurityService');

module.exports.getAllUsers = async (req, res) => {
  const filters = req.query;

  try {
    const users = await UsersService.getUsers(filters);
    return res.send({ users })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.getUser = async (req, res) => {
  const { userId } = req.params;

  try {
    const user = await UsersService.getUser(userId);
    return res.send({ user })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.createUser = async (req, res) => {
  try {
    const tokenData = getUserFromToken(req);

    const userId = await UsersService.createUser(req.body, tokenData?.id, req.userPermissions);
    const user = await UsersService.getUser(userId);

    return res.send({ user })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.modifyUser = async (req, res) => {
  const { userId } = req.params;

  try {
    await UsersService.modifyUser(userId, req.body, req.userPermissions);
    const user = await UsersService.getUser(userId);

    return res.send({ user })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.autocomplete = async (req, res) => {
  const { search } = req.query;

  try {
    const users = await UsersService.autocomplete(search);

    return res.send({ users })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}
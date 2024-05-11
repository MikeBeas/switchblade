const { query, format } = require('../database/query');
const { HTTP } = require('../constants');
const { hashPassword, isAuthorized, getUserFromToken } = require('../services/SecurityService');
const { formatPermissionsForDisplay } = require('../utilities/permissions');
const { version, production } = require('../../config/sys');

module.exports.getRoot = async (req, res) => {
  const authenticated = await isAuthorized(req);
  const user = authenticated ? getUserFromToken(req, true) : { id: null, username: null };
  user.permissions = req.userPermissions;

  return res.send({
    api: {
      host: req.headers.host,
      production,
      authenticated,
      user
    },
    switchblade: {
      version
    },
    features: {
      MULTI_STEP_MFA: true,
      SHORTCUT_KEYWORD_SEARCH: true,
      VERSION_KEYWORD_SEARCH: true,
      USER_PERMISSIONS: true,
      CREATOR_ID_FILTER: true,
      SINCE_VERSION_FILTER: true
    },
    permissions: formatPermissionsForDisplay()
  })
}

module.exports.setup = async (req, res) => {
  const foundUsers = await query(`SELECT user_id FROM users LIMIT 1`);

  if (foundUsers.length > 0) {
    return res.status(HTTP.BAD_REQUEST).send({ message: 'Switchblade has already been set up. You cannot run the setup again.', success: false })
  }

  const { username, password } = req.body;

  if (!username || username.trim() === "" || !password || password.trim() === "") {
    return res.status(HTTP.BAD_REQUEST).send({ message: 'You must specify a username and password', success: false })
  }

  const password_hash = await hashPassword(password);

  const sql = format(`INSERT INTO users SET ?;`, { username, password_hash, is_owner: true });

  const result = await query(sql);

  return res.send({ message: `A new user with username "${username}" was created. The user ID is ${result.insertId}. You have set up your Switchblade server. This function will no longer be available.`, success: true })
}

module.exports.generatePasswordHash = async (req, res) => {
  const { password } = req.body;

  const hashedPassword = await hashPassword(password);

  return res.send({
    hashedPassword,
    message: 'You can set the hashed_password value in the database row for your user to the hashedPassword string attached to this message. This will reset your password to the new value you just entered.'
  })
}
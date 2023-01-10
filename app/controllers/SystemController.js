const { query, format } = require('../database/query');
const { HTTP } = require('../constants');
const { hashPassword } = require('../services/SecurityService');

module.exports.setup = async (req, res) => {
  const foundUsers = await query(`SELECT user_id FROM users LIMIT 1`);

  if (foundUsers.length > 0) {
    return res.status(HTTP.BAD_REQUEST).send({ message: 'Switchblade has already been setup. You cannot run the setup again.' })
  }

  const { username, password } = req.body;

  if (!username || username.trim() === "" || !password || password.trim() === "") {
    return res.status(HTTP.BAD_REQUEST).send({ message: 'You must specify a username and password' })
  }

  const password_hash = await hashPassword(password);

  const sql = format(`INSERT INTO users SET ?;`, { username, password_hash });

  const result = await query(sql);

  return res.send({ message: `A new user with username "${username}" was created. The user ID is ${result.insertId}. You have setup your Switchblade server. This API will no longer be available.` })
}

module.exports.generatePasswordHash = async (req, res) => {
  const { password } = req.body;

  const hashedPassword = await hashPassword(password);

  return res.send({
    hashedPassword,
    message: 'You can set the hashed_password value in the database row for your user to the hashedPassword string attached to this message. This will reset your password to the new value you just entered.'
  })
}
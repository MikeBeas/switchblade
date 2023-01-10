const encryption = require('simple-encryptor')(process.env.ENCRYPTION_KEY);
const argon2 = require('argon2')
const tokenHandler = require('jsonwebtoken');
const { authenticator } = require('otplib');
const { HTTP, AUTH, LINE } = require('../constants');
const { query } = require('../database/query');

const TOKEN_HEADER_PREFIX = "Bearer ";

const calculateTimeout = () => (Date.now() / 1000) + (process.env.JWT_TIMEOUT_IN_SECONDS ?? 3600);

const userIsDeleted = async (userId) => {
  const user = await query(`SELECT user_deleted FROM users where user_id = :userId:`, { userId }, { returnFirst: true });
  return user?.user_deleted ? true : false;
}

module.exports.encrypt = (value) => encryption.encrypt(value)
module.exports.decrypt = (value) => encryption.decrypt(value)

module.exports.login = async (req) => {
  const { body: { username, password, otp }, headers: { host } } = req;

  if (!username || !password) {
    throw new Error("Username and password are required.");
  }

  const user = await query("SELECT user_id, username, password_hash, otp_secret, mfa_enabled FROM users WHERE username = :username: AND user_deleted = FALSE", { username }, { returnFirst: true });

  if (!user?.user_id) {
    throw new Error(AUTH.LOGIN_FAILED);
  }

  const hashedPassword = Buffer.from(user['password_hash'], 'base64').toString();
  const passwordVerified = await argon2.verify(hashedPassword, password, { type: argon2.argon2id })

  if (!passwordVerified) {
    throw new Error(AUTH.LOGIN_FAILED);
  }

  if (user.mfa_enabled) {
    if (!otp || otp.trim() === "") {
      throw new Error("Security code is required.");
    }

    const decryptedSecret = this.decrypt(user.otp_secret);

    try {
      const valid = authenticator.check(otp, decryptedSecret);
      if (!valid) throw new Error(AUTH.LOGIN_FAILED);
    } catch (e) {
      console.log()
      console.log(LINE)
      console.log('MFA secret decryption failed.\nIf your encryption key recently changed, you may need to disable MFA for all users and have them re-enable it.')
      console.log(LINE)
      console.log()
      throw new Error('Failed to decrypt MFA secret. Please contact your Switchblade administrator.')
    }
  }

  const token = issueToken(user, host);

  // no need to await this
  query("UPDATE users SET last_login = NOW() WHERE username = :username:", { username });
  return token;
}

const issueToken = (user, host) => {
  const tokenData = {
    "iss": host,
    "aud": host,
    "iat": Math.floor(Date.now() / 1000),
    "nbf": Math.floor(Date.now() / 1000),
    "exp": Math.floor(calculateTimeout()),
    "data": {
      "id": user.user_id,
      "username": user.username
    }
  }

  return tokenHandler.sign(tokenData, process.env.JWT_KEY, { algorithm: process.env.JWT_ALGO ?? 'HS256' })
}

module.exports.isAuthorized = async (req) => {
  if (!req) return false;
  const auth = req.headers.authorization;
  if (!auth) return false;
  if (auth.includes(TOKEN_HEADER_PREFIX)) {
    const token = auth.split(TOKEN_HEADER_PREFIX)[1] ?? "invalid";

    try {
      const tokenData = tokenHandler.verify(token, process.env.JWT_KEY);
      if (tokenData?.data?.id) {
        return !await userIsDeleted(tokenData.data.id);
      }
    } catch {
      return false;
    }
  } else {
    return false;
  }
}

module.exports.unauthorized = (res) => {
  return res.status(HTTP.NOT_AUTHORIZED).send({ message: AUTH.UNAUTHORIZED });
}

const getTokenFromRequest = (req) => {
  if (!req) return null;
  const auth = req.headers.authorization;
  if (!auth) return null;

  if (auth.includes(TOKEN_HEADER_PREFIX)) {
    return auth.split(TOKEN_HEADER_PREFIX)[1] ?? "invalid";
  } else {
    return null;
  }
}

module.exports.getUserFromToken = (req) => {
  const token = getTokenFromRequest(req);
  if (!token) {
    throw new Error(AUTH.NOT_AUTHORIZED);
  }
  const { data } = tokenHandler.decode(token, process.env.JWT_KEY);
  return data;
}

module.exports.hashPassword = async (password) => {
  const hash = await argon2.hash(password, { type: argon2.argon2id });
  return Buffer.from(hash, 'utf-8').toString('base64');
}
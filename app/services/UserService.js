const { authenticator } = require('otplib');
const qrcode = require('qrcode');
const { query, format } = require('../database/query');
const { hashPassword, encrypt, decrypt } = require('./SecurityService');

const formatUser = (row) => ({
  id: row.user_id,
  username: row.username,
  lastLogin: row.last_login,
  deleted: row.deleted ? true : false
})

const validateUserData = async (userData) => {
  const validators = {
    username: {
      validator: async (username) => {
        if (!username || username.trim() === "") {
          throw new Error("Invalid username")
        }

        return username;
      },
    },
    password: {
      column: 'password_hash',
      validator: async (password) => {
        if (!password || password.trim() === "") {
          throw new Error("Invalid password")
        }

        return await hashPassword(password);
      }
    }
  };

  const validated = {};

  for (const key of Object.keys(validators)) {
    if (Object.keys(userData).includes(key)) {
      const { validator, column } = validators[`${key}`];

      validated[`${column ?? key}`] = await validator(userData[`${key}`])
    }
  }

  return validated;
}

module.exports.getUser = async (userId) => {
  const userData = await query(`SELECT * FROM users WHERE user_id = :userId:`, { userId }, { returnFirst: true });

  if (userData.length === 0) {
    throw new Error("User not found");
  }

  return formatUser(userData)
}

module.exports.modifyUser = async (userId, userData) => {
  const validated = await validateUserData(userData, true);

  const sql = format(`UPDATE users SET ? WHERE user_id = :userId:`, validated);
  await query(sql, { userId });
}

module.exports.beginMfaSetup = async (userId) => {
  const existingSetup = await query(`SELECT username, mfa_enabled FROM users WHERE user_id = :userId:`, { userId }, { returnFirst: true });

  if (existingSetup?.mfa_enabled) {
    throw new Error("Your account already has MFA enabled. Please disable MFA to set it up again.");
  }

  const secret = authenticator.generateSecret();
  const encryptedSecret = encrypt(secret);
  await query(`UPDATE users SET otp_secret = :encryptedSecret: WHERE user_id = :userId:`, { encryptedSecret, userId })

  const url = `otpauth://totp/${existingSetup.username}?secret=${secret}&issuer=Switchblade`;
  const qrCode = await qrcode.toDataURL(url);

  return {
    secret,
    url,
    qrCode,
    message: "MFA is almost setup on your account. Use the secret, QR code, or URL to add the security code to your MFA application, then submit a valid security code to the /me/mfa/complete endpoint to confirm you have setup your MFA application correctly."
  };
}

module.exports.completeMfaSetup = async (userId, body) => {
  const { otp } = body;
  const user = await query(`SELECT otp_secret FROM users WHERE user_id = :userId: AND user_deleted = FALSE`, { userId }, { returnFirst: true });

  if (!user.otp_secret) {
    throw new Error("You need to complete the first step of the MFA setup process before proceeding to this step.")
  }

  const decryptedSecret = decrypt(user.otp_secret);
  const valid = authenticator.check(otp, decryptedSecret);

  if (valid) {
    await query(`UPDATE users SET mfa_enabled = TRUE WHERE user_id = :userId:`, { userId });
  } else {
    throw new Error("The security code you provided was not correct.")
  }
}

module.exports.deleteMfaConfiguration = async (userId) => await query(`UPDATE users SET otp_secret = NULL, mfa_enabled = FALSE WHERE user_id = :userId:`, { userId });
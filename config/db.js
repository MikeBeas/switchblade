const dotenv = require('dotenv');
dotenv.config();

const DB_HOST_OVERRIDE = process.env.SWITCHBLADE_DB_HOST_ENV_VAR ?? 'DB_HOST';
const DB_DATABASE_OVERRIDE = process.env.SWITCHBLADE_DB_DATABASE_ENV_VAR ?? 'DB_NAME';
const DB_USER_OVERRIDE = process.env.SWITCHBLADE_DB_USER_ENV_VAR ?? 'DB_USER';
const DB_PASS_OVERRIDE = process.env.SWITCHBLADE_DB_PASS_ENV_VAR ?? 'DB_PASS';

module.exports = {
  host: process.env[DB_HOST_OVERRIDE],
  database: process.env[DB_DATABASE_OVERRIDE],
  user: process.env[DB_USER_OVERRIDE],
  password: process.env[DB_PASS_OVERRIDE]
}
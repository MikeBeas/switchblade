const mysql = require('mysql2/promise');
const dbConfig = require('../../config/db');

module.exports.format = mysql.format;

const queryFormat = function (query, values) {
  if (!values) return query;

  return query.replace(/:(\w+):/g, function (txt, key) {
    if (Object.keys(values).includes(key)) {
      return this.escape(values[key])
    }
    return txt;
  }.bind(this))
}

const pool = mysql.createPool({
  ...dbConfig,
  connectionLimit: process.env.SWITCHBLADE_DB_CONNECTION_LIMIT ?? 100,
  supportBigNumbers: true,
  queryFormat
})

const _query = async (sqlQuery, args = [], options = {}) => {
  const {
    returnFirst = false // return first row
  } = options;

  let sql = sqlQuery.trim();

  try {
    const [rows] = await pool.query(sql, args);
    return returnFirst ? rows[0] ?? null : rows;
  } catch (e) {
    console.log(e)
    throw new Error(`There was an error with the Switchblade database. ${e.message}`);
  }
}

module.exports.query = (...params) => _query(...params); // enable try/catch
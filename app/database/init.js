const fs = require('fs');
const mysql = require('mysql2/promise');
const dbConfig = require('../../config/db');

module.exports = async () => {
  const connection = await mysql.createConnection({
    ...dbConfig,
    multipleStatements: true
  })

  const addUpdateTable = `
  CREATE TABLE IF NOT EXISTS database_update_history (
    update_id BIGINT NOT NULL AUTO_INCREMENT,
    update_timestamp TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    sql_file_name varchar(40) NOT NULL,
    PRIMARY KEY (update_id),
    KEY sql_file_name_index (sql_file_name)
  )
`

  await connection.query(addUpdateTable);

  const previouslyRan = [];

  const [rows] = await connection.query(`SELECT * FROM database_update_history`);
  for (const row of rows) {
    previouslyRan.push(row.sql_file_name)
  }

  const scripts = fs.readdirSync(`./sql`);

  if (!scripts || !scripts.length) {
    return;
  }

  for (const script of scripts) {
    if (previouslyRan.includes(script)) continue; // skip db updates that already happened

    console.log(`Running database update ${script}`)
    const fileContent = fs.readFileSync(`./sql/${script}`, 'utf8')

    try {
      await connection.query(fileContent);
      await connection.query(`INSERT INTO database_update_history (sql_file_name) VALUES (?)`, [script])
    } catch (e) {
      console.log(e)
    }
  }

  connection.end();
}
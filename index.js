const cluster = require('cluster');
const totalCPUs = require('os').cpus().length;
const config = require('./config/sys');
const databaseInit = require('./app/database/init');
const { query } = require('./app/database/query');
const { LINE } = require('./app/constants');

const PORT_OVERRIDE = process.env.SWITCHBLADE_APP_PORT_ENV_VAR ?? 'PORT';
const port = process.env[PORT_OVERRIDE] ?? 500;

const validateConfiguration = async () => {
  console.log()
  console.log(LINE)
  console.log('Welcome to Switchblade')
  console.log(LINE)
  console.log()
  console.log(LINE)

  console.log('Validating database connection...')
  try {
    // validate database config
    await query(`SELECT 1 as 'one'`, [], { returnFirst: true });
    console.log('Database connected')
  } catch {
    throw new Error('Database connection failed.\nPlease check your environment configuration and try again.')
  }

  // validate jwt secret is set
  console.log('Validating JWT configuration...')
  if (!process.env.JWT_KEY || process.env.JWT_KEY.trim() === "") {
    throw new Error(`You need to configure a JWT_KEY environment variable.\nPlease check the Switchblade documentation for more information.`)
  } else {
    console.log('JWT configuration validated')
  }

  console.log('Validation passed')

  console.log(LINE)
};

const app = require('./app/server');

const boot = async () => {
  if (cluster.isPrimary) {

    await validateConfiguration();

    console.log()
    console.log(LINE)

    console.log('Checking for available database updates...');
    await databaseInit();
    console.log('Database update complete');

    console.log(LINE)
    console.log()

    console.log(LINE)
    console.log(`Running in ${config.production ? 'production' : 'development'} mode`)
    console.log(`Switchblade is ready to use`);
    console.log(`Starting ${totalCPUs} workers...`);
    console.log(LINE)

    console.log()

    for (let i = 0; i < totalCPUs; i++) {
      cluster.fork();
    }

    cluster.on('exit', (worker, code, signal) => {
      console.log(`Worker ${worker.id} (PID ${worker.process.pid}) died with ${signal ? 'signal' : 'code'} ${signal || code}. Starting replacement...`);
      cluster.fork();
    });

  } else {
    app.listen(port, () => {
      console.log(`Started worker ${cluster.worker.id}`);
    });
  }
}

boot()
  .catch((e) => {
    console.log()
    console.log(LINE)
    console.log(e.message)
    console.log(LINE)
    console.log()
    console.log()
    process.stdin.resume(); // prevent crash loop by holding the process open so user can see logs
  });
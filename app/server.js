const express = require('express');
const cors = require('cors');
const enforce = require('express-sslify');

const dotenv = require('dotenv');
dotenv.config();

const app = express();

app.use(express.text());
app.use(express.json());
app.use(express.urlencoded({ extended: true }))
app.use(cors());
if (process.env.NODE_ENV !== "local") app.use(enforce.HTTPS({ trustProtoHeader: true }));
app.set('json spaces', 4);
app.use((req, _, next) => {
  if (!req.headers.system) req.headers.system = "12";
  if (!req.headers.release) req.headers.release = "12.0";
  if (req.body && typeof req.body !== "object") req.body = JSON.parse(req.body);
  next();
})

require('./routes')(app);

module.exports = app;
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

require('./routes')(app);

module.exports = app;
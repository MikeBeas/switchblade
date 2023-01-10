const SecurityService = require('../services/SecurityService');

module.exports = async (req, res, next) => {
  if (await SecurityService.isAuthorized(req)) {
    return next();
  }
  return SecurityService.unauthorized(res);
}
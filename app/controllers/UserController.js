const UserService = require('../services/UserService')
const SecurityService = require('../services/SecurityService');
const { HTTP } = require('../constants');

module.exports.login = async (req, res) => {
  try {
    const token = await SecurityService.login(req);
    return res.send({ token });
  } catch (e) {
    return res.status(HTTP.NOT_AUTHORIZED).send({ message: e.message });
  }
}

// the auth middleware runs before this
module.exports.verify = (_, res) => res.send({ message: "You are authenticated" });

module.exports.getMyIdentity = async (req, res) => {
  const tokenData = SecurityService.getUserFromToken(req);

  try {
    const user = await UserService.getUser(tokenData.id);
    return res.send({ user });
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.modifyMyIdentity = async (req, res) => {
  const tokenData = SecurityService.getUserFromToken(req);

  try {
    await UserService.modifyUser(tokenData.id, req.body);
    const user = await UserService.getUser(tokenData.id);

    return res.send({ user });
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.beginMfaSetup = async (req, res) => {
  const tokenData = SecurityService.getUserFromToken(req);

  try {
    const setup = await UserService.beginMfaSetup(tokenData.id);

    return res.send({ setup });
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.completeMfaSetup = async (req, res) => {
  const tokenData = SecurityService.getUserFromToken(req);

  try {
    await UserService.completeMfaSetup(tokenData.id, req.body);
    return res.send({ message: 'MFA has been setup for your account. If you lose access to your MFA device, the owner of the Switchblade database can remove your MFA configuration manually.' });
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.deleteMfaConfiguration = async (req, res) => {
  const tokenData = SecurityService.getUserFromToken(req);

  try {
    await UserService.deleteMfaConfiguration(tokenData.id);
    return res.send({ message: 'MFA has been removed from your account.' });
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}
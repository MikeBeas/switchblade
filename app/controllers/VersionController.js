const VersionService = require('../services/VersionService');
const ShortcutService = require('../services/ShortcutService');
const { HTTP } = require('../constants');
const { isAuthorized, getUserFromToken } = require('../services/SecurityService');

module.exports.getVersion = async (req, res) => {
  const { shortcutId, versionNumber } = req.params;
  const filters = req.query;
  const authenticated = await isAuthorized(req);
  const user = getUserFromToken(req, true);

  try {
    const shortcut = await ShortcutService.getShortcut(shortcutId, authenticated);
    const version = await VersionService.getVersion(shortcutId, versionNumber, authenticated, filters, { userId: user?.id, permissions: req.userPermissions });
    return res.send({ shortcut, version })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.getHistory = async (req, res) => {
  const { shortcutId } = req.params;
  const filters = req.query;
  const authenticated = await isAuthorized(req);
  const user = getUserFromToken(req, true);

  try {
    const shortcut = await ShortcutService.getShortcut(shortcutId, authenticated);
    const versions = await VersionService.getHistory(shortcutId, authenticated, filters, { userId: user?.id, permissions: req.userPermissions });
    return res.send({ shortcut, versions })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.createVersion = async (req, res) => {
  try {
    const tokenData = getUserFromToken(req);
    const shortcutId = req.params.shortcutId;

    await VersionService.createVersion(shortcutId, req.body, tokenData?.id);

    const shortcut = await ShortcutService.getShortcut(shortcutId, true);
    const version = await VersionService.getVersion(shortcutId, req.body.version, true);

    return res.send({ shortcut, version })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.modifyVersion = async (req, res) => {
  const { shortcutId, versionNumber } = req.params;

  try {
    await VersionService.modifyVersion(shortcutId, versionNumber, req.body);

    const shortcut = await ShortcutService.getShortcut(shortcutId, true);
    const version = await VersionService.getVersion(shortcutId, versionNumber, true);

    return res.send({ shortcut, version })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}
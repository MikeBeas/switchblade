const ShortcutService = require('../services/ShortcutService');
const { HTTP } = require('../constants');
const { isAuthorized, getUserFromToken } = require('../services/SecurityService');

module.exports.getAllShortcuts = async (req, res) => {
  const authenticated = await isAuthorized(req);
  const userId = authenticated ? getUserFromToken(req)?.id ?? null : null;
  const filters = req.query;

  try {
    const shortcuts = await ShortcutService.getAllShortcuts(authenticated, filters, { userId, permissions: req.userPermissions });
    return res.send({ shortcuts })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.getShortcut = async (req, res) => {
  const { shortcutId } = req.params;
  const authenticated = await isAuthorized(req);

  try {
    const shortcut = await ShortcutService.getShortcut(shortcutId, authenticated);
    return res.send({ shortcut })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.createShortcut = async (req, res) => {
  try {
    const tokenData = getUserFromToken(req);

    const shortcutId = await ShortcutService.createShortcut(req.body, tokenData?.id);
    const shortcut = await ShortcutService.getShortcut(shortcutId, true);

    return res.send({ shortcut })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}

module.exports.modifyShortcut = async (req, res) => {
  const { shortcutId } = req.params;

  try {
    await ShortcutService.modifyShortcut(shortcutId, req.body);
    const shortcut = await ShortcutService.getShortcut(shortcutId, true); // user is already authenticated before reaching here

    return res.send({ shortcut })
  } catch (e) {
    return res.status(HTTP.BAD_REQUEST).send({ message: e.message });
  }
}
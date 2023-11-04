const { HTTP, PERMISSIONS } = require('../constants');
const SecurityService = require('../services/SecurityService');
const { getShortcut, userCanAccessShortcut } = require('../services/ShortcutService');
const { userCanAccessShortcutVersion } = require('../services/VersionService');

module.exports.applyUserPermissions = async (req, res, next) => {
  req.userPermissions = await SecurityService.getUserPermissions(req);
  return next();
}

const viewShortcut = async (req, res, next) => {
  try {
    const user = SecurityService.getUserFromToken(req, true);

    if (!await userCanAccessShortcut(user?.id, req.userPermissions, req.params.shortcutId)) {
      throw new Error("Could not find the requested shortcut")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const createShortcut = (req, res, next) => {
  try {
    if (!req.userPermissions[PERMISSIONS.CREATE_SHORTCUTS]) {
      throw new Error("You do not have permission to create shortcuts")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const modifyShortcut = async (req, res, next) => {
  try {
    const user = SecurityService.getUserFromToken(req);

    if (!await userCanAccessShortcut(user?.id, req.userPermissions, req.params.shortcutId)) {
      throw new Error("You do not have permission to modify this shortcut")
    }

    const { creator: { id: shortcutCreatorId } } = await getShortcut(req.params.shortcutId, true); // skip auth here to get the shortcut regardless of who created it

    if (shortcutCreatorId !== user?.id && !req.userPermissions[PERMISSIONS.MODIFY_ANY_SHORTCUTS]) {
      throw new Error("You do not have permission to modify this shortcut")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const viewShortcutVersion = async (req, res, next) => {
  try {
    if (req.params.versionNumber === "latest") {
      return next();
    }

    const user = SecurityService.getUserFromToken(req);

    if (!await userCanAccessShortcutVersion(user?.id, req.userPermissions, req.params.shortcutId, req.params.versionNumber)) {
      throw new Error("You do not have permission to view this version")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const getVersionHistory = async (req, res, next) => {
  try {
    const user = SecurityService.getUserFromToken(req);
    if (!await userCanAccessShortcut(user?.id, req.userPermissions, req.params.shortcutId)) {
      throw new Error("Could not find the requested shortcut")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const createVersion = async (req, res, next) => {
  try {
    const user = SecurityService.getUserFromToken(req);

    if (!await userCanAccessShortcut(user?.id, req.userPermissions, req.params.shortcutId)) {
      throw new Error("You do not have permission to create versions for this shortcut")
    }

    const { creator: { id: shortcutCreatorId } } = await getShortcut(req.params.shortcutId, true); // skip auth here to get the shortcut regardless of who created it

    if (shortcutCreatorId !== user.id && !req.userPermissions[PERMISSIONS.CREATE_VERSION_FOR_ANY_SHORTCUT]) {
      throw new Error("You do not have permission to create versions for this shortcut")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const modifyVersion = async (req, res, next) => {
  try {
    const user = SecurityService.getUserFromToken(req);

    if (!await userCanAccessShortcutVersion(user?.id, req.userPermissions, req.params.shortcutId, req.params.versionNumber)) {
      throw new Error("You do not have permission to modify this shortcut")
    }

    const { creator: { id: shortcutCreatorId } } = await getShortcut(req.params.shortcutId, true); // skip auth here to get the shortcut regardless of who created it

    if (shortcutCreatorId !== user.id && !req.userPermissions[PERMISSIONS.MODIFY_VERSION_FOR_ANY_SHORTCUT]) {
      throw new Error("You do not have permission to modify versions for this shortcut")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const getUsers = async (req, res, next) => {
  try {
    if (!req.userPermissions[PERMISSIONS.VIEW_USERS]) {
      throw new Error("You do not have permission to view users")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const createUser = async (req, res, next) => {
  try {
    if (!req.userPermissions[PERMISSIONS.CREATE_USERS]) {
      throw new Error("You do not have permission to create users")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}

const modifyUser = async (req, res, next) => {
  try {
    const user = SecurityService.getUserFromToken(req);
    const userIdBeingModified = req.params.userId;

    if (userIdBeingModified !== user.id && !req.userPermissions[PERMISSIONS.MODIFY_USERS]) {
      throw new Error("You do not have permission to modify users")
    }
    return next();
  } catch ({ message }) {
    return res.status(HTTP.BAD_REQUEST).send({ message })
  }
}


module.exports.permissions = {
  viewShortcut,
  createShortcut,
  modifyShortcut,
  getVersionHistory,
  viewShortcutVersion,
  createVersion,
  modifyVersion,
  getUsers,
  createUser,
  modifyUser
}
module.exports.LINE = "-----------------------------";

module.exports.HTTP = Object.freeze({
  OK: 200,
  NO_CONTENT: 204,
  BAD_REQUEST: 400,
  NOT_AUTHORIZED: 401,
  NOT_FOUND: 404,
  CONFLICT: 409,
  INTERNAL_ERROR: 500
})

module.exports.AUTH = Object.freeze({
  LOGIN_FAILED: "Invalid login",
  UNAUTHORIZED: "You need to login to use this feature"
})

module.exports.SHORTCUT_STATUS = Object.freeze({
  PUBLISHED: 0,
  DRAFT: 1
})

module.exports.SHORTCUT_STATUS_LABELS = Object.freeze({
  [this.SHORTCUT_STATUS.PUBLISHED]: "Published",
  [this.SHORTCUT_STATUS.DRAFT]: "Draft"
})

module.exports.VERSION_STATUS = Object.freeze({
  PUBLISHED: 0,
  DRAFT: 1
})

module.exports.VERSION_STATUS_LABELS = Object.freeze({
  [this.VERSION_STATUS.PUBLISHED]: "Published",
  [this.VERSION_STATUS.DRAFT]: "Draft"
})

module.exports.FILTER_BOOL = Object.freeze({
  FALSE: [
    "false",
    "0",
    0,
    "f",
    false,
    "no",
    "n"
  ],
  TRUE: [
    "true",
    "1",
    1,
    "t",
    true,
    "yes",
    "y"
  ]
})

module.exports.PLATFORMS = Object.freeze({
  MAC: "mac",
  IOS: "ios"
})

module.exports.SHORTCUT_DEFAULTS = Object.freeze({
  state: this.SHORTCUT_STATUS.PUBLISHED,
  deleted: false
})

module.exports.VERSION_DEFAULTS = Object.freeze({
  minimumiOS: process.env.DEFAULT_MINIMUM_IOS_VERSION ?? 12,
  minimumMac: process.env.DEFAULT_MINIMUM_MAC_VERSION ?? 12,
  state: this.VERSION_STATUS.PUBLISHED,
  deleted: false,
  required: false
})

module.exports.PERMISSIONS = Object.freeze({
  VIEW_ANY_DRAFT_SHORTCUT: "viewAnyDraftShortcut",
  CREATE_SHORTCUTS: "createShortcuts",
  MODIFY_ANY_SHORTCUTS: "modifyAnyShortcut",
  VIEW_DRAFT_VERSIONS_FOR_ANY_SHORTCUT: "viewDraftVersionsForAnyShortcut",
  CREATE_VERSION_FOR_ANY_SHORTCUT: "createVersionForAnyShortcut",
  MODIFY_VERSION_FOR_ANY_SHORTCUT: "modifyVersionForAnyShortcut",
  VIEW_USERS: "viewUsers",
  CREATE_USERS: "createUsers",
  MODIFY_USERS: "modifyUsers"
})
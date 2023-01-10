module.exports.LINE = "-----------------------------";

module.exports.HTTP = {
  OK: 200,
  NO_CONTENT: 204,
  BAD_REQUEST: 400,
  NOT_AUTHORIZED: 401,
  NOT_FOUND: 404,
  CONFLICT: 409,
  INTERNAL_ERROR: 500
}

module.exports.AUTH = {
  LOGIN_FAILED: "Invalid login",
  UNAUTHORIZED: "You need to login to use this feature"
}

module.exports.SHORTCUT_STATUS = {
  PUBLISHED: 0,
  DRAFT: 1
}

module.exports.SHORTCUT_STATUS_LABELS = {
  [this.SHORTCUT_STATUS.PUBLISHED]: "Published",
  [this.SHORTCUT_STATUS.DRAFT]: "Draft"
}

module.exports.VERSION_STATUS = {
  PUBLISHED: 0,
  DRAFT: 1
}

module.exports.VERSION_STATUS_LABELS = {
  [this.VERSION_STATUS.PUBLISHED]: "Published",
  [this.VERSION_STATUS.DRAFT]: "Draft"
}

module.exports.FILTER_BOOL = {
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
}

module.exports.PLATFORMS = {
  MAC: "mac",
  IOS: "ios"
}

module.exports.SHORTCUT_DEFAULTS = {
  state: this.SHORTCUT_STATUS.PUBLISHED,
  deleted: false
}

module.exports.VERSION_DEFAULTS = {
  minimumiOS: process.env.DEFAULT_MINIMUM_IOS_VERSION ?? 12,
  minimumMac: process.env.DEFAULT_MINIMUM_MAC_VERSION ?? 12,
  state: this.VERSION_STATUS.PUBLISHED,
  deleted: false,
  required: false
}
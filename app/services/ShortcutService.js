const { SHORTCUT_STATUS, SHORTCUT_STATUS_LABELS, FILTER_BOOL, SHORTCUT_DEFAULTS, PERMISSIONS } = require('../constants');
const { format, query } = require('../database/query');
const { removeUndefined, cleanString } = require('../utilities/common');

module.exports.formatShortcut = (row = {}) => ({
  id: row.shortcut_id,
  name: row.shortcut_name,
  headline: row.shortcut_headline,
  description: row.shortcut_description,
  website: row.shortcut_website,
  state: {
    value: row.shortcut_state,
    label: SHORTCUT_STATUS_LABELS[row.shortcut_state] ?? "Unknown"
  },
  deleted: row.shortcut_deleted ? true : false,
  creator: {
    id: row.shortcut_created_by,
    name: row.username
  }
});

const validateShortcutData = (shortcutData = {}, updating = false) => {

  const data = updating ?
    shortcutData
    : {
      ...SHORTCUT_DEFAULTS,
      ...shortcutData
    }

  if ((!updating && !data.name) || data.name?.trim() === "") {
    throw new Error("You must specify a shortcut name")
  }

  if (Object.keys(data).includes('state') || !updating) {
    if (!Object.values(SHORTCUT_STATUS).includes(Number(data.state))) {
      throw new Error("The shortcut state you provided was not recognized");
    }
  }

  const returnData = {
    shortcut_name: Object.keys(data).includes('name') ? cleanString(data.name) : undefined,
    shortcut_headline: Object.keys(data).includes('headline') ? cleanString(data.headline) : undefined,
    shortcut_description: Object.keys(data).includes('description') ? cleanString(data.description) : undefined,
    shortcut_website: Object.keys(data).includes('state') ? cleanString(data.website) : undefined,
    shortcut_state: Object.keys(data).includes('state') ? Number(data.state) : undefined,
    shortcut_deleted: Object.keys(data).includes('deleted') ? (data.deleted ? true : false) : undefined
  }

  return updating ? removeUndefined(returnData) : returnData;
}

module.exports.userCanAccessShortcut = async (userId, permissions, shortcutId) => {
  const shortcut = await this.getShortcut(shortcutId, true);
  const { state, deleted, creator } = shortcut;

  if (userId) {
    if (creator.id === userId) {
      return true;
    }

    if ((state.value === SHORTCUT_STATUS.DRAFT || deleted) && !permissions[PERMISSIONS.VIEW_ANY_DRAFT_SHORTCUT]) {
      return false;
    }
  } else {
    if ((state.value !== SHORTCUT_STATUS.PUBLISHED || deleted)) {
      return false;
    }
  }

  return true;
}

module.exports.getShortcut = async (shortcutId, authenticated = false) => {
  const deleted = authenticated ? '' : 'AND shortcut_deleted = false AND shortcut_state = :published:';

  const sql = `SELECT s.*, u.username FROM shortcuts s JOIN users u ON (u.user_id = s.shortcut_created_by) WHERE shortcut_id = :shortcutId: ${deleted};`;
  const row = await query(sql, { shortcutId, published: SHORTCUT_STATUS.PUBLISHED }, { returnFirst: true });

  if (!row) {
    throw new Error("Could not find the requested shortcut");
  }

  return this.formatShortcut(row);
}

module.exports.getAllShortcuts = async (authenticated = false, filters = {}, config = {}) => {
  if (!authenticated) {
    filters = {
      deleted: false,
      state: `${SHORTCUT_STATUS.PUBLISHED}`
    }
  }

  const queryFilters = [];
  const filterValues = {};

  const deletedFilter = filters.deleted;
  if (FILTER_BOOL.TRUE.includes(deletedFilter)) {
    queryFilters.push("shortcut_deleted = true")
  } else if (FILTER_BOOL.FALSE.includes(deletedFilter)) {
    queryFilters.push("shortcut_deleted = false")
  }

  const stateFilter = filters.state?.split(",");
  if (stateFilter && stateFilter.length > 0) {
    const allowedItems = [];
    for (const stateFilterItem of stateFilter) {
      if (stateFilterItem !== "" && Object.values(SHORTCUT_STATUS).includes(Number(stateFilterItem))) {
        allowedItems.push(stateFilterItem)
      }
    }

    if (allowedItems.length > 0) {
      queryFilters.push("shortcut_state IN (:states:)");
      filterValues.states = allowedItems;
    }
  }

  if (filters?.search && filters?.search?.trim() !== "") {
    filterValues.search = `%${filters.search}%`;

    queryFilters.push(`(shortcut_name LIKE :search: OR shortcut_headline LIKE :search: OR shortcut_description LIKE :search:)`);
  }

  if (filters?.creatorId) {
    filterValues.creatorId = filters.creatorId;

    queryFilters.push(`shortcut_created_by = :creatorId:`);
  }

  if (!config.permissions[PERMISSIONS.VIEW_ANY_DRAFT_SHORTCUT]) {
    queryFilters.push(`(shortcut_created_by = :currentUserId: OR (shortcut_state = ${SHORTCUT_STATUS.PUBLISHED} AND shortcut_deleted = false))`);
    filterValues.currentUserId = config.userId;
  }

  const filterString = queryFilters.length > 0 ? `WHERE ${queryFilters.join(" AND ")}` : '';

  const sql = `SELECT s.*, u.username FROM shortcuts s JOIN users u ON (u.user_id = s.shortcut_created_by) ${filterString};`;
  const rows = await query(sql, filterValues);

  return (rows ?? []).map(this.formatShortcut);
}

module.exports.createShortcut = async (shortcutData, userId) => {
  const validated = validateShortcutData(shortcutData);
  validated.shortcut_created_by = userId;

  const sql = format(`INSERT INTO shortcuts SET ?`, validated);
  const result = await query(sql);

  return result.insertId;
}

module.exports.modifyShortcut = async (shortcutId, shortcutData) => {
  const validated = validateShortcutData(shortcutData, true);

  const sql = format(`UPDATE shortcuts SET ? WHERE shortcut_id = :shortcutId:`, validated);
  await query(sql, { shortcutId });
}
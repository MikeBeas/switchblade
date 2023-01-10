const { VERSION_STATUS, VERSION_STATUS_LABELS, FILTER_BOOL, PLATFORMS, SHORTCUT_STATUS, VERSION_DEFAULTS } = require('../constants');
const { format, query } = require('../database/query');
const { removeUndefined } = require('../utilities/common');

module.exports.formatVersion = (row = {}) => ({
  version: row.version_number,
  notes: row.release_notes,
  url: row.download_url,
  minimumiOS: row.minimum_ios_version,
  minimumMac: row.minimum_mac_version,
  released: row.release_date,
  state: {
    value: row.version_state,
    label: VERSION_STATUS_LABELS[row.version_state] ?? "Unknown"
  },
  deleted: row.version_deleted ? true : false,
  required: row.version_required ? true : false,
  prerelease: row.version_is_prerelease ? true : false
});

const validateVersionData = (versionData = {}, updating = false) => {
  const data = updating ?
    versionData
    : {
      ...VERSION_DEFAULTS,
      ...versionData,
    }

  if (!updating) {
    if (!data.version || data.version.trim() === "") {
      throw new Error("You must specify a unique version number for this shortcut");
    }
  }

  if ((!updating && !data.url) || data.url?.trim() === "") {
    throw new Error("You must specify a download URL for this version");
  }

  if (Object.keys(data).includes('state') || !updating) {
    if (!Object.values(VERSION_STATUS).includes(Number(data.state))) {
      throw new Error("The version state you provided was not recognized");
    }
  }

  // do not update version number
  const validated = updating ? {} : {
    version_number: data.version.trim(),
    version_is_prerelease: data.version.includes("-")
  };

  const returnData = {
    ...validated,
    release_notes: data.notes?.trim(),
    download_url: data.url?.trim(),
    minimum_ios_version: Object.keys(data).includes('minimumiOS') ? data.minimumiOS : undefined,
    minimum_mac_version: Object.keys(data).includes('minimumMac') ? data.minimumMac : undefined,
    release_date: data.date?.trim(),
    version_state: Object.keys(data).includes('state') ? Number(data.state) : undefined,
    version_deleted: Object.keys(data).includes('deleted') ? (data.deleted ? true : false) : undefined,
    version_required: Object.keys(data).includes('required') ? (data.required ? true : false) : undefined
  }

  return updating ? removeUndefined(returnData) : returnData;
}

module.exports.createVersion = async (shortcutId, versionData, userId) => {
  const validated = validateVersionData(versionData);

  validated.shortcut_id = shortcutId;
  validated.version_created_by = userId;

  const sql = format(`INSERT INTO shortcut_versions SET ?`, validated);
  const result = await query(sql);

  return result.insertId;
}

module.exports.getVersion = async (shortcutId, version, authenticated = false, filters = {},) => {
  const getLatest = version === "latest";
  const getByVersionNumber = getLatest ? '' : 'AND v.version_number = :version:';

  const filterArray = [];
  if (getLatest) {
    // when getting latest, resepct ios and mac version requirements
    if (filters.platform && filters.platformVersion && Object.values(PLATFORMS).includes(filters.platform)) {
      filterArray.push(`AND minimum_${filters.platform}_version <= :platformVersion:`)
    }

    if (!FILTER_BOOL.TRUE.includes(filters.prerelease)) {
      // do not include prerelease unless explicitly asked for it
      filterArray.push(`AND version_is_prerelease = FALSE`)
    }
  }

  const filterString = filterArray.join(" ");

  const deleted = authenticated ? '' : 'AND v.version_deleted = false AND v.version_state = :versionPublished: AND s.shortcut_deleted = false AND s.shortcut_state = :shortcutPublished:';

  const sql = `SELECT v.* FROM shortcuts s JOIN shortcut_versions v USING (shortcut_id) WHERE v.shortcut_id = :shortcutId: ${getByVersionNumber} ${filterString} ${deleted} ORDER BY v.version_id DESC LIMIT 1;`;

  const row = await query(sql, { shortcutId, version, versionPublished: VERSION_STATUS.PUBLISHED, shortcutPublished: SHORTCUT_STATUS.PUBLISHED, platformVersion: filters?.platformVersion }, { returnFirst: true });

  if (!row) {
    throw new Error("Could not find the requested version");
  }

  return this.formatVersion(row);
}

module.exports.getHistory = async (shortcutId, authenticated = false, filters = {}) => {
  if (!authenticated) {
    filters = {
      deleted: "false",
      state: `${VERSION_STATUS.PUBLISHED}`
    }
  }

  const queryFilters = [];
  const filterValues = { shortcutId };

  const deletedFilter = filters.deleted;
  if (FILTER_BOOL.TRUE.includes(deletedFilter)) {
    queryFilters.push("version_deleted = TRUE")
  } else if (FILTER_BOOL.FALSE.includes(deletedFilter)) {
    queryFilters.push("version_deleted = FALSE")
  }

  if (FILTER_BOOL.TRUE.includes(filters.required)) {
    queryFilters.push(`version_required = TRUE`);
  } else if (FILTER_BOOL.FALSE.includes(filters.required)) {
    queryFilters.push(`version_required = FALSE`);
  }

  if (FILTER_BOOL.TRUE.includes(filters.prerelease)) {
    queryFilters.push(`version_is_prerelease = TRUE`);
  } else if (FILTER_BOOL.FALSE.includes(filters.prerelease)) {
    queryFilters.push(`version_is_prerelease = FALSE`);
  }

  const stateFilter = filters.state?.split(",");
  if (stateFilter && stateFilter.length > 0) {
    const allowedItems = [];
    for (const stateFilterItem of stateFilter) {
      if (Object.values(VERSION_STATUS).includes(Number(stateFilterItem))) {
        allowedItems.push(stateFilterItem)
      }
    }

    if (allowedItems.length > 0) {
      queryFilters.push("version_state IN (:states:)");
      filterValues.states = allowedItems;
    }
  }

  const filterString = queryFilters.length > 0 ? `AND ${queryFilters.join(" AND ")}` : '';

  const sql = `SELECT v.* FROM shortcuts s JOIN shortcut_versions v USING (shortcut_id) WHERE v.shortcut_id = :shortcutId: ${filterString} ORDER BY v.version_id DESC;`;

  const rows = await query(sql, filterValues);

  return (rows ?? []).map(this.formatVersion);
}

module.exports.modifyVersion = async (shortcutId, version, versionData) => {
  const validated = validateVersionData(versionData, true);

  if (Object.keys(validated).length > 0) {
    const sql = format(`UPDATE shortcut_versions SET ? WHERE shortcut_id = :shortcutId: AND version_number = :version:`, validated);

    await query(sql, { shortcutId, version });
  }
}
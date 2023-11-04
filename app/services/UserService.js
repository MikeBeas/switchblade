const { FILTER_BOOL } = require('../constants');
const { format, query } = require('../database/query');
const { cleanString, removeUndefined } = require('../utilities/common');
const { hashPassword, calculateUserPermissions } = require('./SecurityService');

module.exports.formatUser = (row = {}) => ({
  id: row.user_id,
  username: row.username,
  isOwner: row.is_owner ? true : false,
  lastLogin: row.last_login,
  deleted: row.user_deleted ? true : false,
  created: row.user_created,
  creator: {
    id: row.user_created_by,
    username: row.creator_username
  },
  permissions: calculateUserPermissions(row)
});

const validateAllowedToSetPermissions = (permissionsOfUserPerformingUpdate, updatedPermissions = {}) => {
  const validatedPermissions = {};
  Object.keys(updatedPermissions).forEach((key) => {
    if (permissionsOfUserPerformingUpdate[key]) {
      if (updatedPermissions[key] === true) {
        validatedPermissions[key] = true;
      } else if (updatedPermissions[key] === false) {
        validatedPermissions[key] = false;
      }
    }
  })

  return Object.keys(validatedPermissions).length > 0 ? validatedPermissions : undefined;
}

const validateUserData = async (userData = {}, updating = false) => {
  if ((!updating && !userData.username) || userData.username?.trim() === "") {
    throw new Error("You must specify a shortcut name")
  }

  if ((!updating && !userData.password) || userData.password?.trim() === "") {
    throw new Error("You must specify a password during user creation")
  }

  const returnData = {
    username: Object.keys(userData).includes('username') ? cleanString(userData.username) : undefined,
    password_hash: Object.keys(userData).includes('password') ? await hashPassword(userData.password) : undefined,
    user_deleted: Object.keys(userData).includes('deleted') ? (userData.deleted ? true : false) : (updating ? undefined : false)
  }

  return updating ? removeUndefined(returnData) : returnData;
}

module.exports.getUsers = async (filters = {}) => {
  const queryFilters = [];
  const filterValues = {};

  const deletedFilter = filters.deleted;
  if (FILTER_BOOL.TRUE.includes(deletedFilter)) {
    queryFilters.push("u.user_deleted = true")
  } else if (FILTER_BOOL.FALSE.includes(deletedFilter)) {
    queryFilters.push("u.user_deleted = false")
  }

  if (filters?.search && filters?.search?.trim() !== "") {
    filterValues.search = `%${filters.search}%`;

    queryFilters.push(`(u.username LIKE :search:)`);
  }

  const filterString = queryFilters.length > 0 ? `WHERE ${queryFilters.join(" AND ")}` : '';

  const sql = `SELECT u.*, c.username as 'creator_username' FROM users u LEFT JOIN users c ON (c.user_id = u.user_created_by) ${filterString};`;
  const rows = await query(sql, filterValues);

  return (rows ?? []).map(this.formatUser);
}

module.exports.getUser = async (userId) => {
  const sql = `SELECT u.*, c.username as 'creator_username' FROM users u LEFT JOIN users c ON (c.user_id = u.user_created_by) WHERE u.user_id = :userId:;`;
  const row = await query(sql, { userId }, { returnFirst: true });

  if (!row) {
    throw new Error("Could not find the requested user");
  }

  return this.formatUser(row);
}

module.exports.createUser = async (userData, createdByUserId, creatorUserPermissions) => {
  const validated = await validateUserData(userData);
  validated.user_created_by = createdByUserId;

  if (userData.permissions && creatorUserPermissions) {
    validated.user_permissions = JSON.stringify(validateAllowedToSetPermissions(creatorUserPermissions, userData.permissions))
  }

  const sql = format(`INSERT INTO users SET ?`, validated);
  const result = await query(sql);

  return result.insertId;
}

module.exports.modifyUser = async (userId, userData, authenticatedUserPermissions) => {
  const validated = await validateUserData(userData, true);

  if (validated.user_deleted) {
    const user = await this.getUser(userId);
    if (user.isOwner) throw new Error("You cannot delete the server owner");
  }

  if (userData.permissions && authenticatedUserPermissions) {
    validated.user_permissions = JSON.stringify(validateAllowedToSetPermissions(authenticatedUserPermissions, userData.permissions))
  }

  const sql = format(`UPDATE users SET ? WHERE user_id = :userId:`, validated);
  await query(sql, { userId });
}

module.exports.autocomplete = async (search) => {
  if (!search) return [];

  const searchTerm = `%${search}%`;

  const rows = await query(`SELECT user_id, username FROM users WHERE username LIKE :searchTerm:`, { searchTerm });

  return rows.map((u) => ({ id: u.user_id, username: u.username }));
}
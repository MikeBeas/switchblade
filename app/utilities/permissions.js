const { PERMISSIONS } = require('../constants');

module.exports.flattenPermissions = (permissionGroups = [], isOwner = false) => {
  const permissions = {};
  for (const group of permissionGroups) {
    for (const permission of group.permissions) {
      permissions[permission.key] = isOwner ? true : permission.value;
    }
  }

  return permissions;
}

module.exports.formatPermissionsForDisplay = (permissionsFromStorage = {}, isOwner = false) => {
  const template = this.getUserPermissionsTemplate();

  for (const [groupIndex, permissionGroup] of template.entries()) {
    for (const [permissionIndex, permission] of permissionGroup.permissions.entries()) {
      if (permissionsFromStorage[permission.key]) {
        template[groupIndex].permissions[permissionIndex].value = isOwner ? true : permissionsFromStorage[permission.key];
      }
    }
  }

  return template;
}

module.exports.getUserPermissionsTemplate = () => [
  {
    label: "Shortcuts",
    key: "shortcuts",
    permissions: [
      {
        key: PERMISSIONS.VIEW_ANY_DRAFT_SHORTCUT,
        label: "View Any Draft Shortcut",
        value: false
      },
      {
        key: PERMISSIONS.CREATE_SHORTCUTS,
        label: "Create Shortcuts",
        value: false
      },
      {
        key: PERMISSIONS.MODIFY_ANY_SHORTCUTS,
        label: "Modify Any Shortcut",
        value: false
      }
    ]
  },
  {
    label: "Versions",
    key: "versions",
    permissions: [
      {
        key: PERMISSIONS.VIEW_DRAFT_VERSIONS_FOR_ANY_SHORTCUT,
        label: "View Draft Versions for Any Shortcut",
        value: false
      },
      {
        key: PERMISSIONS.CREATE_VERSION_FOR_ANY_SHORTCUT,
        label: "Create Versions for Any Shortcut",
        value: false
      },
      {
        key: PERMISSIONS.MODIFY_VERSION_FOR_ANY_SHORTCUT,
        label: "Modify Versions for Any Shortcut",
        value: false
      }
    ]
  },
  {
    label: "Users",
    key: "users",
    permissions: [
      {
        key: PERMISSIONS.VIEW_USERS,
        label: "View User List",
        value: false
      },
      {
        key: PERMISSIONS.CREATE_USERS,
        label: "Create Users",
        value: false
      },
      {
        key: PERMISSIONS.MODIFY_USERS,
        label: "Modify Users",
        value: false
      }
    ]
  }
];
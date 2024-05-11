# Changelog

## v1.3.0 (2024-05-11)

### New Filter for Shortcut History and Latest Version
This update adds a new filter to several endpoints that can help you get a list of versions that have been released since your user last updated. When you use this filter, you will get back a `versions` array that includes previous versions of the shortcut that were released between the `sinceVersion` and the latest version.

This makes it easy to surface a list of other updates that your users have missed if they are several versions behind.

- Added `sinceVersion` parameter to the `GET /shortcuts/{shortcutId}/version/{versionNumber}` endpoint
  - When used with this endpoint, the API response will include a `versions` array that includes a reverse-chronological list of all of the versions released after the `sinceVersion` and before the `versionNumber`. The `sinceVersion` will not be included in the array, but the latest version will be.
- Added `sinceVersion` parameter to the `GET /shortcuts/{shortcutId}/version/latest` endpoint
  - Behind the scenes, this endpoint is the same as `GET /shortcuts/{shortcutId}/version/{versionNumber}` endpoint, and simply uses `latest` as a magic string that always looks up the newest version. When the new filter is used with this endpoint, the `versions` array will include all updates that have passed since the `sinceVersion` was released. The `sinceVersion` will not be included in the array.
- Added `sinceVersion` parameter to the `GET /shortcuts/{shortcutId}/history` endpoint
  - When the `sinceVersion` filter is used with this endpoint, the history returned will only include versions that were released after the specified `sinceVersion`. It will not include the `sinceVersion` in the results.

### Filter Behavior Correction
Some filters were being incorrect ignored on unauthenticated requests to the `GET /shortcuts/{shortcutId}/history` endpoint. Now only the `state` and `deleted` filters will be overridden when unauthenticated. Also added a note in the README description of the `state` filter for `GET /shortcuts/{shortcutId}/history` that authentication is required, mirroring the similar filter on `GET /shortcuts`.

### Dependency Update
This version includes the latest version of the app's dependencies.

### Chores
- Updated an error in the README that used the description of the `GET /shortcuts/{shortcutId}/history` endpoint in the section about the `GET /shortcuts/{shortcutId}/version/{versionNumber}` endpoint.
- Updated Postman collection with new filter for endpoints listed above
- Updated README with documentation of new filter

## v1.2.1 (2024-02-13)

### Dependency Update
This version includes the latest version of the app's dependencies.

### NPM Version Actually Updated
The previous change log said that `package.json` had been updated to specify npm v10. However, this change was missing. It has been made in this version.

## v1.2.0 (2023-10-04)

### Security Notice!
This update will grant owner-level access to the user your database with the user ID `1`. There is almost certainly no situation in which this user is not your main user, since multi-user access was not officially supported yet.

However, if you have manually added more users to your database, ensure that the first user who was added to your database is your administrator. The two fields to pay attention to are the new `is_owner` and `user_permissions` fields in the `users` table. If you need to copy these values from the user with ID `1` to a different user, do so immediately after starting your Switchblade server after the update, and be sure to reset those values to their defaults on the old user once you have copied them to the correct administrator.

In the future, the `is_owner` field will be used to apply new permissions to your owner user, so these manual changes will not be necessary.

### Node Version Updated to v20
This version of Switchblade specifies Node v20 and npm v10 in package.json. It should still run on v19, but v19 is no longer under maintenance. Node v20 is now the LTS version.

### Multi-User Support and Permissioning System
- Added the ability to get the identity of other users
  - Use the new `GET /users` endpoint to list the users on your Switchblade server
    - This endpoint supports several filters. You can learn more in the README.
  - Use the new `GET /users/{userId}` endpoint to fetch a different user's identity
  - Only the owner of the Switchblade server can view this data by default, but other users can be granted access to it.
- Added the ability to add new users
  - Use the new `POST /users` endpoint to create users
- Added permissions system for managing what users are allowed to do
  - Use the new `PATCH /users/{userId}` endpoint to manage a user, including updating their username, password, and permissions. Only the owner of the Switchblade server will have access to these tools by default, but permission to access the tools can be granted to other users.
  - See what permissions are available using the `GET /` endpoint, which now includes a template of all permissions grouped into categories.
- At this time there is no granular control for allowing users to have admin access to specific shortcuts that they don't own. Users can manage shortcuts/versions they created, and users with the appropriate permission can manage *all* shortcuts or versions.
- Support for more granular control (i.e., allowing the owner of a shortcut to designate a collaborator for that one shortcut so that they can modify it or its versions) is being considered for a future update, but is not guaranteed.
- A new `creatorId` filter has been added to the relevant endpoints for finding shortcuts and versions created by a specific user
- A new `/autocomplete/users` endpoint has been added for getting user-search autocomplete results

### Chores
- Added user identity API to Postman collection and README
- Added user list API to Postman collection and README
- Added user creation API to Postman collection and README
- Added user modification API to Postman collection and README
- Added permissions template API to Postman collection and README
- Added user autocomplete API to Postman collection and README
- Added `creatorId` parameter to relevant endpoints in Postman collection and README
- Updated dependencies

## v1.1.0 (2023-05-20)

This release is accompanied by the release of the [Switchblade SDK](https://github.com/MikeBeas/switchblade-sdk), a JavaScript/TypeScript package that makes it easy to interact with Switchblade servers.

Also being launched with this update is [Switchblade UI](https://github.com/MikeBeas/switchblade-ui), a front end application for managing your Switchblade server and shortcuts. You can run Switchblade UI anywhere you can run React apps.

Switchblade SDK started its life as part of Switchblade UI before being broken out into a separate package so that anyone can use it!

### New (Optional) Multi-Factor Authentication Flow
- Added the ability to perform MFA as a two-step process more in line with how other websites operate
- In version previous versions the MFA code had to be submitted at the same time as the username and password.
- With the new flow, you can submit just the username and password. You will get back a response with a new `mfaRequired` property that indicates you should surface the MFA input to your user, along with an `mfaToken` that is used as part of the next step.
- Submit the MFA code along with a standard `Authorization` header using the `mfaToken` as your bearer token. This `mfaToken` is valid for 45 seconds after creation.
- You will get back the standard login response after this second step.
- The old workflow of submitting the MFA code with the username and password is still fully supported
- This new flow can be detected with the `MULTI_STEP_MFA` feature flag.

### New Search Filters
- Added support for `search` query parameter on `GET /shortcuts` endpoint. This field searches the full text of the shortcut name, headline, and description. This can be detected with the `SHORTCUT_KEYWORD_SEARCH` feature flag.
- Added support for `search` query parameter on `GET /shortcuts/{shortcutId}/history` endpoint. This field searches the full text of the version number, change log, and download URL. This can be detected with the `VERSION_KEYWORD_SEARCH` feature flag.

### Feature Flags on Root
- The root endpoint `GET /` now returns an object indicating which newer features (such as the aforementioned multi-step MFA flow) are available on the server. This can be used for feature detection in front end applications.

### Response Clarity Improvements
- Added `mfaEnabled` as a property on the user object from the `GET /me` endpoint
- The response during the final step of enabling MFA on a user's account will include new `success` boolean to indicate if the operation was successful
- The `POST /setup` endpoint will now return a `success` boolean along with the message to indicate whether or not the setup was successful

### Bug Fixes
- Fixed an issue with the `state` filters on `GET /shortcuts` and `GET /shortcuts/{shortcutId}/history` that could cause them to return only published items if the filter was left blank
- Fixed a bug where Mac and iOS system versions would be written as `0` to the database if not included in requests to `POST /shortcuts/{shortcutId}/version` or `PATCH /shortcuts/{shortcutId}/version/{versionNumber}`
- Fixed an issue that could cause a message about a decryption failure to appear if your OTP code had expired.

### Housekeeping
- Updated README with documentation for new search parameters and MFA flow
- Updated Postman collection with search parameters and support for automatically setting the `mfaToken` in your environment when performing a multi-step MFA flow
- Updated dependencies

---

## v1.0.1 (2023-04-26)

### Chores
- Removed unused middleware
- Added missing query parameter docs to Postman collection
- Updated dependencies

---

## v1.0.0 (2023-01-09)

### Release
- Added everything
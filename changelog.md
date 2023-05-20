# Changelog

## v1.1.0 (TBD)

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
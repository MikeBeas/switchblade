# Switchblade

A self-hosted Apple Shortcuts distribution system.

You can find the changelog here: https://github.com/MikeBeas/switchblade/blob/master/changelog.md

# User Interface and Dev Tools

Switchblade is provided as a "headless" system, which means it does not come with a UI out of the box. You can easily use the API with apps like [Postman](https://www.postman.com/downloads/) and [Insomnia](https://insomnia.rest/download), but thanks to the open and documented API, anyone can build a UI for Switchblade.

Developers who want to create front ends for this software (admin panels, product pages, etc.) can do so as they see fit. Front ends may be distributed any way the creator chooses.

## Switchblade UI
I have also released a separate front end application called [Switchblade UI](https://github.com/MikeBeas/switchblade-ui) that you are free to use if you'd like, or you can build your own or use one developed by another creator. You can run it anywhere you can run a React app.

The design of Switchblade means that users who want to build sites that use it can easily swap out front ends whenever they want, almost like WordPress themes, and be confident that the backend infrastructure remains intact. Shortcut makers can also easily integrate data from Switchblade into their own websites via the API.

## Switchblade SDK

Developers who are interested in building UIs, or shortcut creators who would like to integrate Switchblade API calls into their own websites, can use the [Switchblade SDK](https://github.com/MikeBeas/switchblade-sdk), a JavaScript/TypeScript package that provides an easy interface for making these calls.

Since the front end and back end systems can run completely separately, your front end only needs to know the domain of your Switchblade API (environment variables are recommended for this) and it will be able to reach it from anywhere. So, for example, you could run both your front end and back end on DigitalOcean, or run your back end on Heroku and your front end on Netlify. You could even build a shortcut that uses the Switchblade API to manage your shortcuts. The choice is yours.

# Table of Contents

Use the table of contents button in the upper left corner of the README header on GitHub to navigate this document quickly.


# Getting Started

## Requirements

- A MySQL database (MySQL 8 recommended)
  - In order to be as compatible as possible with different deployment systems, Switchblade does not assume that you have, or can have, a database running locally on the same machine as the main application.
  - The database schema will be created and managed by Switchblade, so there is no need to populate any tables in advance.
  - The database can be hosted anywhere you want as long as it is secure and accessible from your Node runtime.
- Node v19
  - Switchblade is built on the latest version of Node. It may be compatible with older versions, but when given the option to choose your version, go with version 19+ to ensure maximum compatibility.
  - There are many platform-as-a-service (PAAS) providers allow you to deploy a Node application. Switchblade has been tested successfully on Heroku but should run anywhere Node is available.

## Installation

The exact method of installing Switchblade may vary based on your deployment strategy. You may opt to clone the git repo locally, check out the desired version, and push it to your remote deployment service directly using something like Heroku Git or AWS Elastic Beanstalk, for example.

Other deployment strategies may work better if you fork the Switchblade repo on GitHub and connect your fork to your deployment service. You can then pull whatever version of Switchblade you want to run into your master branch and let your deployment service build that version.

Unfortunately, due to the wide variety of possible deployment platforms, it is impossible to provide specific directions for any particular deployment strategy or PAAS. There are many tutorials online that can help you get a Node application up and running on your favorite PAAS.

If you prefer to deploy the application in a Docker container, a Dockerfile is provided that can be used to create a fully functional Switchblade container. You will need to supply the container with the required environment variables at run time, described in the section on [environment configuration](#environment-configuration) below.

Note that Switchblade automatically enforces HTTPS. If you are running the app locally in a Docker container, make sure to set `NODE_ENV=local` in your environment variables to bypass the HTTPS enforcement. You should not use this setting in a cloud environment, since the app needs HTTPS to ensure the security of your password and other information.

To run the app locally outside of a Docker container, just follow these steps:
- Clone the git repo
- `cd` into the repo and run `npm i` to install the dependencies (only needed once)
- Run `npm run dev` to start a dev version of the app. It will automatically restart if you make any code changes. It is not recommended that you make changes to the code, however, since this may make updating to future versions difficult.

When running the app via `npm run dev`, the `NODE_ENV` will automatically be set to `local` for you. You can configure your database connection and other environment variables by placing a `.env` file inside the repo. It has been git-ignored and will not be synced into GitHub.

Running Switchblade locally is really only necessary if you are building a front end and want to test against development data without impacting a real Switchblade instance running on a server somewhere.

## Environment Configuration

Switchblade will need a few environment variables configured in order to connect to your database and support logging in. This section will outline these environment variables and show you how to add an admin user to the system.

### Database Connection

There are four key environment variables that are needed to connect to your MySQL database.

- `DB_HOST`: The AP address or hostname where your database can be found.
- `DB_NAME`: The name of the database to use.
- `DB_USER`: The username to use to connect to the database. It is recommended that you create a user specifically for Switchblade that can only access the Switchblade database, and does not have access to any other databases on the same server.
- `DB_PASS`: The password for the user that will be used to connect to the database.

If you are using a PAAS that provides pre-configured database addons, such as Heroku, and those addons automatically configure environment variables that can be used to connect to the database, you should verify if the environment variables used by that addon match up with those required by Switchblade.

For example, rather than `DB_HOST`, your addon might use `DATABASE_HOST` as the environment variable that contains the database IP.

If the environment variables used by your database addon do not match the required variables, do NOT simply copy the values from the automated variables into new variables with the proper names. Sometimes these automated database addons rotate credentials for security reasons, and any rotation of credentials would not be reflected in the separate environment variables you created, which would cause Switchblade to lose its database connection.

Instead, you can point Switchblade to the correct variables by overriding the required ones above. You can use these keys to do this:

- `SWITCHBLADE_DB_HOST_ENV_VAR`
- `SWITCHBLADE_DB_DATABASE_ENV_VAR`
- `SWITCHBLADE_DB_USER_ENV_VAR`
- `SWITCHBLADE_DB_PASS_ENV_VAR`

So, following our example above where your database addon uses `DATABASE_HOST` instead of `DB_HOST`, you could simply setup your environment variables this way:

```
DATABASE_HOST="some.automated.value.from.your.addon"
SWITCHBLADE_DB_HOST_ENV_VAR="DATABASE_HOST"
```

This will tell Switchblade that it should use the `DATABASE_HOST` value instead of looking for `DB_HOST` to get the hostname or IP address for the database.

### Other Environment Variables

The following environment variables should also be configured before using Switchblade:

- `JWT_KEY`: This is a secret value used in the generation and validation of JWTs when authenticating users. This should be a long, secure string. You can use a password generator to create a secure value. If this value is compromised, it would enable a malicious actor to forge an authentication token as any user in your system. This would grant them full access to your Switchblade API without the need for your username, password, or MFA code. This secret should be impossible to guess and never shared with anyone. If you ever need to change this value, any JWTs created using the old value will be immediately invalidated and unable to be used for authentication.
- `ENCRYPTION_KEY`: This is a secret value used to encrypt MFA secrets in the database. Like the JWT key, this should be a very long, secure string. A password generator is recommended for creating this value. The encryption key must be at least 16 characters long. Using a shorter key will cause Switchblade to fail to launch. If this key is ever compromised, users who gain access to your database can use it to decrypt the MFA secrets stored in the `users` table, which, when used in conjuction with user passwords, would allow them to login to the app as a compromised user. Passwords are not stored in the database, only irreversible hashes of the passwords, so it is not possible for the malicious actor to get your password simply by compromising the database. If your encryption key ever changes, MFA will stop working for all users. You will have to disable MFA for anyone who has it enabled and have them set it up again.

If both of the above values are set with secure values, compromising your Switchblade API would require all of the following:

- Malicious actor must have read access to your database to get the MFA secrets
- Malicious actor must have access to your encryption key to decrypt your MFA secrets
- Malicious actor must be able to crack your password, or find your password in a leak from a different website (always use a separate, distinct, and secure password for each website)

Alternatively:

- Malicious actor must have access to your JWT key only

Always keep the above environment variables secure and secret!

The environment variables below are optional and allow you to further customize aspects of Switchblade.

- `JWT_ALGO`: This allows you to change the algorithm used for creating and verifying JWTs when a user logs in. The default is `HS256`. You can set it to any value supported by [jsonwebtoken](https://www.npmjs.com/package/jsonwebtoken).
- `JWT_TIMEOUT_IN_SECONDS`: This will change how long a JWT is valid for after being issued. When a JWT expires, the user must login again. The default setting is 3600, which is one hour.
- `SWITCHBLADE_DB_CONNECTION_LIMIT`: Switchblade uses a pool of database connections to read and write data to MySQL. You can set this to whatever connection limit you prefer for this pool. The default setting is 100.
- `DEFAULT_MINIMUM_IOS_VERSION`: Minimum iOS versions are explained later in this document. This value will allow you to set the default minimum iOS version for a new shortcut version that is created without an explicitly-defined value. The default value is 12, the first version with the Shortcuts app.
- `DEFAULT_MINIMUM_MAC_VERSION`: Minimum Mac versions are explained later in this document. This value will allow you to set the default minimum Mac version for a new shortcut version that is created without an explicitly-defined value. The default value is 12, the first version with the Shortcuts app.
- `PORT`: This controls what port the application runs on. The default is 500. Generally you should not set this yourself unless running Switchblade locally, as your deployment platform may set it automatically. If your deployment platform configures a port automatically using an environment variable other than `PORT` (this case should be extremely rare), you can override this as described in the database section above using the `SWITCHBLADE_APP_PORT_ENV_VAR` variable.

## Database Schema Management

Switchblade manages your database schema for you by running database update scripts on startup. These scripts are stored in the `./sql` folder of the git repo.

Each time a script is executed against your database, it is logged in a special update tracking table within the database. Once a script has been logged, it will not be run again. You should not modify this table. Doing so may lead to Switchblade incorrectly running scripts again, and could result in data loss.

You should not modify the schema of your database in any way.

The developer of this software is not responsible for any data loss resulting from the use of this software, whether correctly or incorrectly. Always make regular backups of your database, especially before performing Switchblade updates that include schema changes. If you downgrade your installed version of Switchblade, your database schema will not be downgraded. This may result in incompatibility between your database and the software.

During the first startup of Switchblade, the app will create all of the necessary tables for operation. In future updates, additional columns or tables may be added (or dropped) as needed. Always read the change log for each Switchblade update before upgrading your system to understand what you are installing.

## Initial User Creation

You should not share the URL to your Switchblade server until you have created your initial user. If you share your Switchblade domain before completing this step, it could allow a malicious actor to create the first user instead. If this were to happen, you could manually set the `deleted` boolean on the malicious user's account to `true` in the `users` table to prevent them from logging in.

Once you have configured your environment and gotten Switchblade up and running, you can verify that the service is available by visiting the URL for your server, or making a `GET` request to the root of the domain (`/`).

This should return some basic configuration information about your Switchblade installation, including the hostname, version number, and whether or not you are authenticated.

If you do not see the expected result, check the logs for your application and see if there are any messages indicating a problem. Switchblade validates its configuration during startup to ensure you have correctly setup the required environment variables, and will not start if you have not done so.

If you see the server information when hitting this endpoint, you can setup your first user by making a `POST` request to the `/setup` route with the following body:

```json
{
  "username": "YOUR_USERNAME_HERE",
  "password": "YOUR_SECURE_PASSWORD_HERE"
}
```

Be sure to use a strong password (a password generator is recommended). This call will immediately create a new user in your database with the selected username and password. Note that usernames are limit to 50 characters.

As long as there are rows in the `users` table of your database, this API will not do anything and will return an error if anyone tries to use it again. Do not delete rows from your `users` table to prevent abuse of this API.

You are now ready to start using Switchblade! If you want to turn on multi-factor authentication for additional account security, see the next section. Otherwise, you can begin adding shortcuts and versions to your database.

## Optional: Setting Up Multi-Factor Authentication (MFA)

Switchblade supports time-based one-time passwords for multi-factor authentication. You have likely used this type of authentication with Google Authenticator, Authy, or other password and security code management apps.

To enable MFA on your account, follow these steps:

First, make an authenticated (see the section on [authenticating with Switchblade](#authenticating-with-switchblade)) `POST` request to `/me/mfa/setup` with no body. You will get back a response with the following body. Do not share the contents of this response with anyone!

```json
{
  "secret": "some_string",
  "url": "otpauth://a_url_here",
  "qrCode": "data:image/png;base64,some_base_64_here",
  "message": "MFA is almost setup on your account. Use the secret, QR code, or URL to add the security code to your MFA application, then submit a valid security code to the /me/mfa/complete endpoint to confirm you have setup your MFA application correctly."
}
```

You can use these values in the following ways. You will only need to do one of these.

- `secret`: This is your MFA shared secret. If your MFA app has a field to paste in a secret key while adding an account, this is the value you paste there.
- `url`: This URL can open any compatible app that supports one-time passwords and add your account automatically.
- `qrCode`: This data URL will display a QR code when viewed in a web browser. You can scan this with your MFA app to add your account.

Once you have used one of the options above to add the MFA account to your security application, you will need to verify that the app is setup correctly to finish enabling MFA on your account.

To verify the setup worked, get the limited-time code shown in your security app and submit an authenticated `POST` request to `/me/mfa/complete`. You should include this body:

```json
{
  "otp": "YOUR_SIX_DIGIT_CODE_HERE"
}
```

No recovery codes are generated when MFA is setup. Given that the primary use case for this service is a single user or close collaborators working together, it is not infeasible for the administrator to simply reset the `otp_secret` and `mfa_enabled` values in the database for a given user and let them set MFA up again to regain access to their account.

# Authenticating with Switchblade

To log a user in, make a `POST` call to the `POST /login` API endpoint. Use the following values for the body:

```json
{
  "username": "USERNAME_HERE",
  "password": "PASSWORD_HERE"
}
```

If logging in as a user with multi-factor authentication enabled, you can also include a your one-time password as shown:

```json
{
  "username": "USERNAME_HERE",
  "password": "PASSWORD_HERE",
  "otp": "123456"
}
```

In Switchblade version 1.0.0, if a user with MFA enabled attempts to login without providing the MFA code, the request will be rejected with an error message. It is recommended if you are building a Switchblade login form that you provide an optional MFA field alongside the username and password fields so that the user can enter all of this information at the same time.

In later versions of Switchblade, you can perform an MFA-enabled login as a two-step process. This flow works as follows:

First, submit the username and password to the login endpoint:

```json
{
  "username": "USERNAME_HERE",
  "password": "PASSWORD_HERE"
}
```

You will get back a response in the following format:

```json
{
  "mfaRequired": true,
  "mfaToken": "A_JWT_HERE"
}
```

When `mfaRequired` is true in the response, you can use the `mfaToken` to complete the login. Submit another call to the `/login` endpoint with an `Authorization: Bearer MFA_TOKEN_HERE` header, and the OTP value in the body:

```json
{
  "otp": "OTP_HERE"
}
```

The multi-step MFA flow has [feature flag](#feature-flags) of `MULTI_STEP_MFA`.

Upon a successful login through either of these paths, the API will respond with a JSON Web Token (JWT) that identifies the user to the server. This token will be required for any authenticated API calls.

The JWT will eventually expire (see the section on [environment variables](#other-environment-variables) for information on setting how long they take to expire) and need to be replaced by logging in again. Any API request that requires authentication will respond with a `401` HTTP status and a message indicating that the user is not logged in.

When a user is logged in, you may authenticate any API call by including an `Authorization` header on the request with a value of `Bearer {token}` where `{token}` is the user's JWT (do not include the braces `{}`).

Any call that behaves differently when authenticated will have the alternate behavior documented. Some calls require authentication.

# Feature Flags

After Switchblade's initial launch, new features may be added from time to time that require changes the API, such as the addition of new endpoints. You can check for the availability of these features on the server from your frontend by making a request to the `GET /` endpoint and checking the `features` for the relevant flag.

`features` is a key/value list of advanced features supported by the current version of Switchblade. The value for all enabled features will be `true`.

## Current Feature Flags

- `MULTI_STEP_MFA`: The multi-step MFA flow described in the section on [authenticating with the Switchblade API](#authenticating-with-switchblade) is available. It is recommended that when this feature is available, you only surface the MFA field if necessary. For versions without this feature, surface the MFA field for all users on the login screen.

# Boolean API Filters Values

Some APIs support filters that accept boolean values. To set one of these filters to `true`, you may use an of the following values:

- true
- 1
- t
- yes
- y

For `false`, you can use any of these values:

- false
- 0
- f
- no
- n

# Shortcut and Version Objects

## Shortcut

The shortcut object is returned in the following shape:

```json
{
  "id": 1,
  "name": "Test Shortcut",
  "headline": "A cool test shortcut!",
  "description": "This is a very cool test shortcut that you can use for testing things! Here is what you can do with it:\n\n - Test things\n - That's all",
  "state": {
    "value": 0,
    "label": "Published"
  },
  "deleted": false
}
```

- `id`: The shortcut ID. It can be used to modify the shortcut and get more information from other APIs.
- `name`: The name of the shortcut.
- `headline`: A short description of the shortcut.
- `description`: A longer description of the shortcut, suitable for the body of a product page. This may use whatever formatting you want to use on your front end, such as plaintext, Markdown, HTML, or anything else.
- `website`: The URL for a product page where users can learn more about the shortcut.
- `state`: This object includes a `value` and `label` for the current state of the shortcut. Available states are `0` for `Published` and `1` for `Draft`.
- `deleted`: Boolean value indicating whether the shortcut has been deleted.

## Version

The version object is returned in the following shape:

```json
{
  "version": "2.0",
  "notes": "This is a full rewrite that contains breaking changes",
  "url": "https://www.icloud.com/shortcuts/shortcut_id",
  "minimumiOS": 14,
  "minimumMac": null,
  "released": "2023-01-01T05:00:00.000Z",
  "state": {
    "value": 0,
    "label": "Published"
  },
  "deleted": false,
  "required": true,
  "prerelease": false
}
```

- `version`: The version number for this release.
- `notes`: The release notes for this version.
- `url`: The iCloud URL to download this version of the shortcut.
- `minimumiOS`: This is the oldest version of iOS that can run this shortcut. Generally the version of iOS the shortcut was created on. If this value is null, this version will never be shown when the platform query parameter is set to `ios`.
- `minimumMac`: This is the oldest Mac operating system that can run this shortcut. Generally the operating system the shortcut was created on. If this value is null, this version will never be shown when the platform query parameter is set to `mac`.
- `released`: A datetime string with timezone that shows when the shortcut was released. This value is set by the shortcut publisher, it is not automatically generated.
- `state`: This object includes a `value` and `label` for the current state of the version. Available states are `0` for `Published` and `1` for `Draft`.
- `deleted`: Boolean value indicating whether the shortcut has been deleted.
- `required`: Boolean value indicating whether the shortcut is a required update. The UpdateKit API can return this value to clients checking for updates, and they can use it to determine if they should allow the end user to skip the version or not.
- `prerelease`: Boolean value indicating whether the shortcut is an alpha/beta/release candidate/other prerelease version. This is determined automatically based on whether the version number contains a `-`.

# Postman Collection

A Postman collection is available in this repo. Import `SwitchbladePostmanCollection.json` to get started. All APIs are documented with descriptions, all available query parameters, body parameters, authorization headers, and more.

To setup the Postman collection, add a new environment to Postman with the following variables:

- `username`: Your Switchblade username.
- `password`: Your Switchblade password.
- `token`: This will be populated automatically for you. You can leave it blank.
- `host`: The domain of your Switchblade server (no trailing slash).

When you use the login endpoint, the tests setup in Postman will automatically save the token from the response into your environment variables for use in other requests.

# Available Endpoints

This section will document all available endpoints and their uses and behaviors. It is divided into sections based on what type of data the endpoint handles, such as shortcut data, user data, or version data.

## Setup and Management

### `POST /setup`
This endpoint is used to create the first user in the database. Its use is documented in the section on [initial user creation](#initial-user-creation).

### `POST /hash-password`
For security, passwords are not stored directly in the Switchblade database. Instead, they are stored as an irreversible hash. If you want to add another user to your database, you will need to add a row to the `users` table manually. This will require you to have a password hash for that user. You (or the user) can obtain the hash by sending it to this endpoint:

```json
{
  "password": "YOUR_PASSWORD_HERE"
}
```

The API will return a hash that can be added to the `password_hash` column of the `users` table. You should only need to do this if you are trying to add a second administrator to your database. See the section on [multi-user support](#multi-user-support) for more information on multi-user uses.


## Core Functionality

### `GET /`
The root endpoint returns basic configuration information about the server, including the Switchblade software version.

### `POST /login`
This is the API for logging in a registered user. See the section on [authenticating with Switchblade](#authenticating-with-switchblade) for more information on this endpoint.

### `GET /verify`
This endpoint can be used to validate authentication. It will return a success message when you reach it while authenticated and an error message when you reach it without authentication, whether due to an invalid, missing, or expired JWT.


## Current User Account

### `GET /me`
Requires authentication. Gets basic information about the current user's identity, including their user ID, username, and last login timestamp.

### `POST /me/mfa/setup`
Requires authentication. Begins the MFA setup process. See the section on [MFA](#optional-setting-up-multi-factor-authentication-mfa) for more information.

### `POST /me/mfa/complete`
Requires authentication. Completes the MFA setup process. See the section on [MFA](#optional-setting-up-multi-factor-authentication-mfa) for more information.

### `DELETE /me/mfa`
Requires authentication. Deletes the current user's MFA configuration and disables MFA on their account.

### `PATCH /me`
Requires authentication. Allows the current user to modify their profile information. All body parameters are optional. Usernames are limited to 50 characters.

```json
{
  "username": "CHOOSE_NEW_USERNAME",
  "password": "CHOOSE_NEW_PASSWORD"
}
```

## Shortcuts

### `GET /shortcuts`
Gets a list of all shortcuts available on the service. Deleted shortcuts and shortcuts in the draft state will not be returned. When authenticated, deleted and draft shortcuts are returned along with published shortcuts.

The following parameters can be added to the query string to filter what will be returned.

- `deleted`: Requires authentication. Set to `true` to show only deleted shortcuts. Set to `false` to hide deleted shortcuts. Omit to show all shortcuts.
- `state`: Requires authentication. Specify the number value for any supported shortcut state, such as `0` for published and `1` for draft, to see only shortcuts in that state. Supports multiple comma-separated values, such as `?state=0,1`.
- `search`: Searches the full text of the shortcut name, headline, and description fields to find matches. This requires Switchblade 1.1.0 or newer and can be detected using the `SHORTCUT_KEYWORD_SEARCH` [feature flag](#feature-flags).

### `GET /shortcuts/{shortcutId}`
Gets the details for a specific shortcut. When unauthenticated, draft and deleted shortcuts will return an error. When authenticated, draft and deleted shortcuts will return as expected.

### `POST /shortcuts`
Requires authentication. Creates a new shortcut record. The following parameters are available in the request body.

- `name`: Required. The name of the shortcut. This value must be unique. Two shortcuts cannot have the same name.
- `headline`: Optional. The description of your shortcut. Character limit is 255 characters.
- `description`: Optional. The description of your shortcut. Character limit is 65,535 characters. You should be mindful of the fact that the longer this is, the longer it will take for your users to download and the more strain it will place on your system.
- `state`: Optional. Allows you to set the state of the shortcut at creation time. See the [shortcut object details](#shortcut) for available options. Use the desired number value, such as 0 or 1.
- `deleted`: Optional. Allows you to set a shortcut to the deleted state upon creation. If you need that for some reason.

### `PATCH /shortcuts/{shortcutId}`
Allows you to modify an existing shortcut. You can modify an of the parameters used in the [create shortcut endpoint](#post-shortcuts).


## Versions

### `GET /shortcuts/{shortcutId}/version/{versionNumber}`
Gets a list of all versions available for a particular shortcut. Deleted shortcuts/versions and shortcuts/versions in the draft state will not be returned. When authenticated, deleted and draft shortcuts/versions are returned along with published shortcuts/versions. Note that this API uses the version number, not the version ID. Version IDs are never used in any API and only exist for internal database ordering.

### `GET /shortcuts/{shortcutId}/history`
Gets a list of all available versions for a specific shortcut. When unauthenticated, draft and deleted shortcuts/versions will not be included. When authenticated, draft and deleted shortcuts/versions will return as expected.

The following parameters can be added to the query string to filter what will be returned.

- `prerelease`: Set to `true` to see only pre-release versions. Set to `false` to hide pre-release versions. Omit to see all versions.
- `deleted`: Requires authentication. Set to `true` to show only deleted versions or shortcuts. Set to `false` to hide deleted versions or shortcuts. Omit to show all versions.
- `state`: Specify the number value for any supported version state, such as `0` for published and `1` for draft, to see only versions in that state. Supports multiple comma-separated values, such as `?state=0,1`.
- `required`: Set to `true` to see only versions that have been marked as mandatory. Set `false` to exclude mandatory versions. Omit filter to see all versions.
- `search`: Searches the full text of the version number, release notes, and download URL fields to find matches. This requires Switchblade 1.1.0 or newer and can be detected using the `VERSION_KEYWORD_SEARCH` [feature flag](#feature-flags).

### `GET /shortcuts/{shortcutId}/version/latest`
Gets the details for the latest version available for a specific shortcut. When unauthenticated, draft and deleted shortcuts will return an error. When authenticated, draft and deleted shortcuts will return as expected.

The following parameters can be added to the query string to filter what will be included as the latest version.

- `platform`: Can be used to specify which platform the shortcut is intended to run on. Must be combined with `platformVersion` to have any effect. Possible values are `ios` and `mac`.
- `platformVersion`: This should be a major iOS or Mac release number (i.e. 12, 13, etc.). When combined with `platform`, only the latest shortcut version compatible with that version of that operating system or higher will be returned. For example, if there are four versions of a shortcut that support iOS 12, 14, 14 again, and 16, respectively, specifying `?platform=ios&platformVersion=14` would result in the latest version that supports iOS 14 or older. In this case, the second shortcut that supports iOS 14 would be returned as the latest available for that device.
- `prerelease`: If this is set to `true`, it will be possible to get a prerelease version as the latest available version if there is a newer prerelease version than the latest stable version. If this is not set, or is set to false, prerelease versions will not be shown as the latest. This can be combined with other filters, so you could query specifically for any version compatible with iOS 14 or older, including any potential prerelease versions.

### `POST /shortcuts/{shortcutId}/version`
Requires authentication. Creates a new shortcut version record. The following parameters are available in the request body.

- `version`: Required. The version number for this release. This value must be unique to the shortcut. A shortcut cannot have two versions with the same version number. If you include a `-` in the version number, such as `1.0.1-beta.1`, the version will automatically be flagged as a prerelease. Character limit is 255 characters.
- `notes`: Optional. The release notes for this version. This should describe the changes users can expect in the update. Character limit is 65,535 characters. You should be mindful of the fact that the longer this is, the longer it will take for your users to download and the more strain it will place on your system.
- `url`: Required. The iCloud URL to download this version. Character limit is 255.
- `minimumiOS`: Optional. An integer for the oldest version of iOS that can run this shortcut. The default is 12 and can be configured using [environment variables](#other-environment-variables).
- `minimumMac`: Optional. An integer for the oldest version of the Mac operating system that can run this shortcut. The default is 12 and can be configured using [environment variables](#other-environment-variables).
- `date`: Optional. The release date for this version. It will be included in the version metadata if set. If you do not set this explicitly, it will be left blank.
- `required`: Optional. The UpdateKit API will report this value back to any client checking for updates and allow them to handle a required update differently if needed. For example, your shortcut might look at this value to determine if the user should see a "Cancel" or "Skip" button or not when prompted to update, or if they will only see a button to install the update. This can be useful when an update is required for a shortcut to continue functioning, such as when a server the shortcut uses has changed and requires a different type of request that the shortcut is being updated to support.
- `state`: Optional. Allows you to set the state of the version at creation time. See the [version object details](#version) for available options. Use the desired number value, such as 0 or 1.
- `deleted`: Optional. Allows you to set a version to the deleted state upon creation. Just in case.

### `PATCH /shortcuts/{shortcutId}/version/{versionNumber}`
Allows you to modify an existing shortcut version. You can modify an of the parameters used in the [create version endpoint](#post-shortcutsshortcutidversion), with the exception of the `version` parameter, which cannot be changed after creation.

# UpdateKit API Integration

Because Switchblade and the [UpdateKit API](https://www.mikebeas.com/updatekit-api) are developed together, Switchblade is a first-class citizen of the UpdateKit API. In fact, when these two are used together, they offer the most robust prerelease shortcut version support of any shortcut distribution platform.

## Integrating with the UpdateKit API

Using Switchblade with the UpdateKit API is as simple as any other provider. You just need to provide a `module` and `id` as outlined in the UpdateKit API documentation. For Switchblade, the module is `switchblade`. The `id` for your shortcut will be a combination of the Switchblade server domain and the shortcut ID from your Switchblade API joined together by a colon `:`. Note that if your Switchblade API uses `http` rather than `https`, you will need to specify that.

For example, if you run your Switchblade server at `https://switchblade.example.com` and you have a shortcut with the shortcut ID of 1234, you would use these details for the UpdateKit API:

```json
{
  "version": "1.0",
  "module": "switchblade",
  "id": "switchblade.example.com:1234"
}
```

If `switchblade.example.com` did not support SSL, you would specify the ID as `http://switchblade.example.com:1234` with the protocol included. If you do not include the protocol, the UpdateKit API assumes `https`.

All of the other options available on the UpdateKit API will work exactly as expected, and in some cases better than expected, with Switchblade.

## Prerelease Versions

With other shortcut gallery websites, there is a known problem with distributing prerelease versions and stable versions together. Although the UpdateKit API has made an effort to make this effortless, the biggest limitation is on the end of the gallery sites.

Take this example: say that you are running version 1.0 of a shortcut, and versions 1.1 and 1.2-alpha.1 are available. Because other gallery sites only return the single most recent shortcut when checking for updates with no regard for whether it's a prerelease version or not, the UpdateKit API will see 1.2-alpha.1 as the most recent version. If you have opted out of seeing prerelease versions for your shortcut, you will not see any update available, even though version 1.1 is technically available.

Switchblade and the UpdateKit API work around this by using the filters available on the `GET /shortcut/{shortcutId}/version/latest` endpoint. When the UpdateKit API sees that you have opted in to prerelease versions, it queries your Switchblade server for the latest version with prereleases enabled. If you have not opted in to prereleases, it will query Switchblade without that parameter, and will get back version 1.1 in our example above instead. This will ensure that users always see the latest version available to them, whether they only want stable builds or prefer to try experimental options.

No other gallery currently provides this functionality.

# Multi-User Support

If you would like to have collaborators on your Switchblade server, it is possible to add more users. Be aware that this is not the primary intended use of the software, and there are currently no access controls available to prevent collaborators from having total control over the shortcuts on your server. Make sure you fully trust anyone you give access.

Note that some groundwork has been laid for potential future updates that support a true multi-user system with user sign ups, the ability for end users to specify collaborators on specific shortcuts with access controls, multiple admin/moderator users, and more. However, real multi-user/community features are not actually planned for development at this time, and would only ever be possible if there was a real demand for them.

To create a new user, use the [`POST /hash-password` endpoint](#post-hash-password) described above to create a hash of the user's chosen password. Then log into your database management app and manually insert a new row into the `users` table. You only need to specify the `username` and `password_hash` fields. Everything else will populate automatically. Once you've done this, the user can login via the API and begin using your Switchblade server.

Once again, ensure you trust anyone you give this kind of access. While they will not be able to access your database directly, they will have unrestricted access to all authenticated APIs.

To remove a user, you can either delete their row from the `users` table (not recommended -- remember that having zero users will re-enable the setup API and allow anyone to add an admin user to your database) or flip the `deleted` boolean to `true` for that user. Disabled users will immediately be unable to access authenticated endpoints, even if their JWT has not expired.

# Building a UI for Switchblade

Anyone is free to build a UI on top of the Switchblade backend, and distribute that UI in any way they want, including for money. Below you will find some things to keep in mind when building a UI.

- When designing your login screen, remember that MFA is optional and must be submitted at the same time as the username and password
- You should consider supporting the following pages at minimum:
  - Login
  - Setup (where you can submit a username and password to generate the first user on the server)
  - Shortcut list
  - New shortcut page
  - Manage shortcut page (with list of versions)
  - New version page
  - Manage version page
  - Draft support for shortcuts and versions
  - User settings page with support for updating username and password, as well as enabling or disabling MFA
- You may also want to provide a simple unauthenticated front end for the password hashing API so site owners can use it to reset their password if necessary.
- Product pages would be a good inclusion, but are not necessary if you are just building a management UI. Some users may want to use their own website to show off their shortcuts and may not need product pages in their Switchblade UI, while others may not have the web design or development expertise to build their own pages and would prefer to rely on their Switchblade UI to provide such pages.
- You can get the current installed version number of the Switchblade backend from the root of the Switchblade API. This can be used for feature detection. As new features are added to the API, additional feature detection capabilities may be added. Any feature detection capabilities will be documented in this README at the time they are added. This will allow you to write UIs that can show or hide different elements based on whether the backend supports that feature.
- You should support an environment variable, such as `SWITCHBLADE_DOMAIN` (or similar, as supported by your front end build tools) that allows the user to set the domain for their backend. You can use this for making API calls to Switchblade. You should not expect the user to modify your code to set one of your constants to their domain name.

# Not Planned/Out of Scope

Below is a list of features that might be desirable for a shortcut community site but which are not currently planned for Switchblade in any capacity, along with explanations for why each is not planned.

## Comments

Dealing with situations like spam and moderation makes this a very unappealing feature to attempt. Commenting is probably best handled by implementing existing commenting systems like [Disqus](https://disqus.com) in a front end package rather than building it from scratch.

## Password resets via email (or any other email features)

Not planned, although it does feel in scope. The big issue here is that with the "run anywhere" mentality, it's hard to say if Switchblade admins or the hardware running Switchblade will have access to things like SMTP servers. Asking users to sign up for a third-party email provider like SendGrid or Mailchimp to get an API key feels wrong and creates another dependency that admins have to deal with (and I have to write code to support). I'm not sure yet how I'll handle this if true multi-user support is built.
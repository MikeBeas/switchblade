CREATE TABLE IF NOT EXISTS users (
  `user_id` BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT,
  `username` VARCHAR(50) NOT NULL,
  `password_hash` VARCHAR(2500) NOT NULL,
  `otp_secret`  VARCHAR(255) NULL,
  `mfa_enabled` TINYINT(1) NOT NULL DEFAULT 0,
  `last_login` TIMESTAMP NULL,
  `user_deleted` TINYINT(1) NOT NULL DEFAULT 0,
  `user_created` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  UNIQUE INDEX `username` (`username`)
);

CREATE TABLE IF NOT EXISTS shortcuts (
  `shortcut_id` BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT,
  `shortcut_name` VARCHAR(255) NOT NULL,
  `shortcut_headline` VARCHAR(255) NULL,
  `shortcut_description` TEXT NULL,
  `shortcut_website` VARCHAR(255) NULL,
  `shortcut_state` TINYINT NOT NULL DEFAULT 0,
  `shortcut_deleted` TINYINT(1) NOT NULL DEFAULT 0,
  `shortcut_created` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `shortcut_created_by` BIGINT NOT NULL,
  UNIQUE INDEX `shortcut_name` (`shortcut_name`),
  FOREIGN KEY (`shortcut_created_by`) REFERENCES `users` (`user_id`) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS shortcut_versions (
  `version_id` BIGINT NOT NULL PRIMARY KEY AUTO_INCREMENT,
  `shortcut_id` BIGINT NOT NULL,
  `version_number` VARCHAR(255) NOT NULL,
  `release_notes` TEXT NULL,
  `download_url` VARCHAR(255) NOT NULL,
  `minimum_ios_version` INT NULL,
  `minimum_mac_version` INT NULL,
  `release_date` TIMESTAMP NULL,
  `version_state` TINYINT(1) NOT NULL DEFAULT 0,
  `version_is_prerelease` TINYINT(1) NOT NULL DEFAULT 0,
  `version_required` TINYINT(1) NOT NULL DEFAULT 0,
  `version_deleted` TINYINT(1) NOT NULL DEFAULT 0,
  `version_created` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `version_created_by` BIGINT NOT NULL,
  UNIQUE INDEX `shortcut_version` (`shortcut_id`,`version_number`),
  FOREIGN KEY (`shortcut_id`) REFERENCES `shortcuts` (`shortcut_id`) ON DELETE CASCADE,
  FOREIGN KEY (`version_created_by`) REFERENCES `users` (`user_id`) ON DELETE RESTRICT
);
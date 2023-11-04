ALTER TABLE users
  ADD COLUMN is_owner BOOL NOT NULL DEFAULT '0' AFTER password_hash,
  ADD COLUMN user_permissions JSON NULL AFTER mfa_enabled,
  ADD COLUMN user_created_by BIGINT NULL;

UPDATE users SET is_owner = true WHERE user_id = 1;
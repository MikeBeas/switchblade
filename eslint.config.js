const js = require('@eslint/js');
const globals = require('globals');

module.exports = [
  js.configs.recommended,
  {
    rules: {
      "no-empty": 0
    },
    languageOptions: {
      sourceType: "module",
      ecmaVersion: 2021,
      globals: {
        ...globals.node,
        Atomics: "readonly",
        SharedArrayBuffer: "readonly"
      }
    }
  }
]
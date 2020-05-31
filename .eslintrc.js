module.exports = {
  env: {
    browser: true,
    es6: true,
    node: true,
  },
  extends: [
    `google`,
    `eslint:recommended`,
    `plugin:react/recommended`,
    `prettier`,
    `prettier/react`,
  ],
  globals: {
    Atomics: `readonly`,
    SharedArrayBuffer: `readonly`,
  },
  parserOptions: {
    ecmaFeatures: {
      jsx: true,
    },
    ecmaVersion: 11,
    sourceType: `module`,
  },
  plugins: [
    `filenames`,
    `prettier`,
    `react`,
  ],
  rules: {
    "filenames/match-regex": [`error`, `^[a-z-\\d\\.]+$`, true],
    "quotes": [`error`, `backtick`],
    "react/jsx-key": `warn`,
    "react/prop-types": `off`,
    "require-jsdoc": `off`,
    "valid-jsdoc": `off`,
  },
  settings: {
    react: {
      version: `16.4.2`,
    },
  },
};

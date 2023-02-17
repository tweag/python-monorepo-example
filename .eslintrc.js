// Lint rules and style based on
// https://github.com/gatsbyjs/gatsby/tree/master/.eslintrc.js.

module.exports = {
  parser: `@typescript-eslint/parser`,
  env: {
    browser: true,
    es6: true,
    node: true,
  },
  extends: [
    `google`,
    `eslint:recommended`,
    `plugin:@typescript-eslint/eslint-recommended`,
    `plugin:@typescript-eslint/recommended`,
    `plugin:react/recommended`,
    `prettier`,
  ],
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
    `@typescript-eslint/eslint-plugin`,
  ],
  rules: {
    "filenames/match-regex": [`error`, `^[a-z-\\d\\.]+$`, true],
    "prettier/prettier": `error`,
    quotes: [`error`, `backtick`],
    "react/jsx-key": `warn`,
    "react/prop-types": `off`,
    "require-jsdoc": `off`,
    "valid-jsdoc": `off`,
    "react/no-unknown-property": `off`,
  },
  settings: {
    react: {
      version: `16.4.2`,
    },
  },
}

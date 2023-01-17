/* eslint camelcase: 0 */

import { css } from "@emotion/react"

const buttonSize = {
  small: `
    padding: 7px 20px;
    font-size: 14px;
  `,
  medium: `
    padding: 9px 18px;
    font-size: 18px;
    line-height: 1.2;
  `,
  large: `
    padding: 15px 18px;
    font-size: 24px;
    line-height: 1.2;
  `,
}

const buttonVariant = {
  buttonLinkBottomLined: `
    ::after {
      content: "";
      display: block;
      width: 0;
      height: 2px;
      background: #000;
      transition: width 0.4s;
      margin-top: 2px;
    }

    :hover::after {
      width: 100%;
      //transition: width .4s;
    }
  `,
}

export const globalStyles = t => {
  return css`
    :root {
      --bg-color: white;
      --fg-color: black;
    }
    img::selection {
      background-color: rgba(255, 255, 255, 0) !important;
    }
    img::-moz-selection {
      background-color: rgba(255, 255, 255, 0) !important;
    }
    tbody tr:nth-child(odd) {
      background-color: ${t.colors.grey};
    }
    table {
      margin-bottom: 2em;
      border-collapse: collapse;
    }
    th {
      font-weight: bold;
    }
    th,
    td {
      padding: 5px 15px 5px 15px;
      min-width: 120px;
      max-width: 240px;
    }
    tr,
    td,
    th {
      vertical-align: middle;
    }
    svg {
      width: 100%;
      height: 100%;
      padding: 0;
      margin: 0;
    }
    samp {
      white-space: pre-wrap;
      word-wrap: break-word;
      text-align: justify;
    }
    .clearfix:after {
      content: "";
      display: block;
      clear: both;
    }
    .ellipsis {
      white-space: nowrap; /* 1 */
      text-overflow: ellipsis; /* 2 */
      overflow: hidden;
    }
    html {
      box-sizing: border-box;
    }
    *,
    *:before,
    *:after {
      box-sizing: inherit;
    }
    * {
      max-height: 1000000px;
    }

    /* Transitions
	 ========================================================================== */

    /*   for react-full-page */
    .transition--slide-fade-in {
      -webkit-transition: transform 0.9s ease 0.3s, opacity 0.9s ease 0.3s,
        visibility 0.9s ease 0.3s;
      transition: transform 0.9s ease 0.3s, opacity 0.9s ease 0.3s,
        visibility 0.9s ease 0.3s;
      opacity: 0;
      visibility: hidden;

      &.delayed {
        transition-delay: 0.8s;
      }

      ${[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map(
        num => `
        &.delayed-${num} {
          transition-delay: ${(num * 0.1).toFixed(1)}s;
        }
      `
      )}

      &.bottom-in {
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }

      &.top-in {
        -webkit-transform: translateY(-300px);
        -ms-transform: translateY(-300px);
        transform: translateY(-300px);
      }

      &.right-in {
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
      }

      &.left-in {
        -webkit-transform: translateX(-300px);
        -ms-transform: translateX(-300px);
        transform: translateX(-300px);
      }

      ${t.breakpoints.map((v, i) => {
        return `
          &.only-above-${i} {
            @media only screen and (max-width: ${v}) {
              transform: none;
              opacity: 1;
              visibility: visible;
            }
          }
        `
      })}

      /* No transitions in percy testing. */
      @media only percy {
        transform: none;
        opacity: 1;
        visibility: visible;
      }
    }

    .section:not(.active) {
      .transition--slide-fade-in {
        transition-delay: 0s;
      }
    }

    .section.active {
      .transition--slide-fade-in {
        transform: none;
        opacity: 1;
        visibility: visible;
      }
    }

    .transition-section__transition--slide-fade-in {
      -webkit-transition: transform 0.9s ease 0.3s, opacity 0.9s ease 0.3s;
      transition: transform 0.9s ease 0.3s, opacity 0.9s ease 0.3s;
      opacity: 0;
      visibility: hidden;

      &.delayed {
        transition-delay: 0.8s;
      }

      &.duration-3 {
        transition-duration: 0.3s;
      }

      ${[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12].map(
        num => `
        &.delayed-${num} {
          transition-delay: ${(num * 0.1).toFixed(1)}s;
        }
      `
      )}

      &.bottom-in {
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }

      &.top-in {
        -webkit-transform: translateY(-300px);
        -ms-transform: translateY(-300px);
        transform: translateY(-300px);
      }

      &.right-in {
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
      }

      &.left-in {
        -webkit-transform: translateX(-300px);
        -ms-transform: translateX(-300px);
        transform: translateX(-300px);
      }

      ${t.breakpoints.map((v, i) => {
        return `
          &.only-above-${i} {
            @media only screen and (max-width: ${v}) {
              transform: none;
              opacity: 1;
              visibility: visible;
            }
          }
        `
      })}

      &.min-0--none {
        @media only screen and (min-width: ${t.breakpoints[0]}) {
          transform: none;
          opacity: 1;
          visibility: visible;
        }
      }

      /* No transitions in percy testing. */
      @media only percy {
        transform: none;
        opacity: 1;
        visibility: visible;
      }
    }

    .transition-section:not(.in-viewport) {
      .transition-section__transition--slide-fade-in {
        transition-delay: 0s;
      }
    }

    .transition-section.in-viewport {
      .transition-section__transition--slide-fade-in {
        transform: none;
        opacity: 1;
        visibility: visible;
      }
    }

    /* Transitions end.
	 ========================================================================== */

    /* New refactored css.  */

    /* Used on blog tag buttons, in footer, etc. */
    .button {
      display: inline-block;

      &.button-lined {
        text-decoration: underline;
      }

      &.button-small {
        ${buttonSize.small}
      }

      &.button-medium {
        ${buttonSize.medium}
      }

      &.button-large {
        ${buttonSize.large}
      }

      &.min-5__button-large {
        @media (min-width: ${t.breakpoints[2]}) {
          ${buttonSize.large}
        }
      }

      &.button-link {
        transition: all 0.4s ease;

        &:not(:visited) {
          color: var(--fg-color);
        }

        &:hover {
          background: var(--fg-color);
          color: var(--bg-color);
        }
      }

      &.button-link-bottom-lined {
        ${buttonVariant[`buttonLinkBottomLined`]}
      }
      &.min-1__button-link-bottom-lined {
        @media (min-width: ${t.breakpoints[1]}) {
          ${buttonVariant[`buttonLinkBottomLined`]}
        }
      }

      &.button-secondary {
        border: 1px solid var(--fg-color);
        border-radius: 35px;
        text-align: center;
        color: var(--fg-color);
        background: linear-gradient(to right, black 50%, transparent 50%);
        background-size: 200% 100%;
        background-position: right bottom;
        transition: all 0.5s ease;

        &.inverted {
          border: 1px solid var(--fg-color);
          color: var(--fg-color);
          background: linear-gradient(to right, white 50%, transparent 50%);
          background-size: 200% 100%;
          background-position: right bottom;
          transition: all 0.5s ease;
        }

        &.pre-arrow-right {
          &::before {
            display: inline-block;
            vertical-align: top;
            font-family: "icomoon" !important;
            content: "\\e900";
            margin: 1px 4px 0 0;
          }
        }

        &:hover {
          color: var(--bg-color);
          background-position: left bottom;
        }
      }
    }

    /* ========================================================================== */
    body {
      color: var(--fg-color);
      background-color: var(--bg-color);
      font: 16px/1.2 "stratos", "Arial", "Helvetica", sans-serif;
      min-width: 320px;
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
    }
    *::selection,
    *::-moz-selection {
      background: var(--fg-color);
      color: var(--bg-color);
    }
    img {
      max-width: 100%;
      height: auto;
      vertical-align: top;
    }
    a:link {
      text-decoration: none;
    }

    /* Used for arrows before a customer name in testimonials, etc. */
    [class^="icon-"],
    [class*=" icon-"] {
      /* use !important to prevent issues with browser extensions that change fonts */
      font-family: "stratos" !important;
      font-size: inherit;
      font-style: normal;
      font-weight: normal;
      font-variant: normal;
      text-transform: none;
      line-height: 1; /* Better Font Rendering =========== */
      -webkit-font-smoothing: antialiased;
      -moz-osx-font-smoothing: grayscale;
    }
    .icon-arrow-right:before {
      content: "→";
    }
    .icon-arrow-right1:before {
      content: "→";
    }

    /* Needed for background logos on the open source categories, etc. */
    .section {
      position: relative;
    }

    /* Various color definitions */
    .s_white {
      --bg-color: white;
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_yellow {
      --bg-color: ${t.colors.yellow};
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_purple {
      --bg-color: ${t.colors.purple};
      --fg-color: white;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_blue {
      --bg-color: ${t.colors.blue};
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_black {
      --bg-color: black;
      --fg-color: white;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_orange {
      --bg-color: ${t.colors.orange};
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_green {
      --bg-color: ${t.colors.green};
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_beige {
      --bg-color: ${t.colors.beige};
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_grey {
      --bg-color: ${t.colors.grey};
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }
    .s_red {
      --bg-color: ${t.colors.red};
      --fg-color: black;
      background-color: var(--bg-color);
      color: var(--fg-color);
    }

    @media only percy {
      .hide-in-percy {
        visibility: hidden;
      }
    }
  `
}

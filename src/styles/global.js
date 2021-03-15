/* eslint camelcase: 0 */

import { css } from "@emotion/core"

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

      &.min-1--none {
        @media only screen and (min-width: ${t.breakpoints[1]}) {
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
        @media (min-width: ${t.breakpoints[5]}) {
          ${buttonSize.large}
        }
      }

      ${[5].reduce(
        (acc, breakpoint) => `${acc}
            &.min-${breakpoint}__button-large {
              @media (min-width: ${t.breakpoints[breakpoint]}) {
                ${buttonSize.large}
              }
            }
          `,
        ``
      )}

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
    h1,
    .h1,
    h2,
    .h2,
    h3,
    .h3,
    h4,
    .h4,
    h5,
    .h5,
    h6,
    .h6,
    .h {
      font-family: inherit;
      font-weight: bold;
      margin: 0 0 0.5em;
    }
    p {
      margin: 0 0 1em;
    }
    a:link {
      text-decoration: none;
    }
    body.menu-active .menu a:after {
      top: 45px;
    }

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

    #wrapper {
      position: relative;
      overflow: hidden;
      width: 100%;
    }
    a:link {
      -webkit-transition: all 0.4s ease;
      transition: all 0.4s ease;
    }

    .section {
      position: relative;
    }

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

    main > .fp-section:last-child .fp-tableCell {
      vertical-align: top;
      background: black; // Match the footer down to bottom of viewport
    }

    h1 {
      font-size: ${t.fontSizes[8]};
      line-height: 1;
      font-weight: 700;
      text-transform: uppercase;
      margin: 0;
    }
    h2 {
      font-size: ${t.fontSizes[7]};
      line-height: 1;
      font-weight: 700;
      text-transform: uppercase;
      margin: 0;
    }
    h3 {
      font-size: ${t.fontSizes[5]};
      line-height: 1.1;
      font-weight: 700;
      margin: 0;
    }

    main nav ul li a {
      color: var(--fg-color);
    }
    main nav ul li a:hover {
      color: var(--fg-color);
      text-decoration: underline;
    }

    /* code section overrides */
    main nav {
      width: 65%;
      max-width: 1000px;
      margin: 0 0 30px;
      padding: 0 0 0 120px;
    }

    @media (min-width: 1400px) {
      h2 {
        font-size: ${t.fontSizes[6]};
      }
      h3 {
        font-size: ${t.fontSizes[5]};
      }
    }
    @media (max-width: 1499px) {
      main nav {
        padding: 0 0 0 60px;
      }
      h2 {
        font-size: ${t.fontSizes[5]};
      }
      h3 {
        font-size: ${t.fontSizes[4]};
      }
    }
    @media (max-width: 767px) {
      h1 {
        font-size: ${t.fontSizes[5]};
        min-height: 100px;
      }
      h2 {
        font-size: ${t.fontSizes[3]};
      }

      h3 {
        font-size: ${t.fontSizes[2]};
      }
      main nav {
        margin: 30px 15px !important;
        padding: 0;
      }

      main nav {
        margin: 20px 15px;
        padding: 0 15px;
        width: 100%;
      }
      .section {
        padding: 0;
        margin-top: 0;
      }
      #___gatsby {
        overflow: hidden;
      }
    }
    @media only percy {
      .hide-in-percy {
        visibility: hidden;
      }
    }
  `
}

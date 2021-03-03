/* eslint camelcase: 0 */

import { css } from "@emotion/core"

import frise_biotech from "../images/frise_biotech.svg"
import frise_blog_home from "../images/frise_blog_home.svg"
import frise_blog_post from "../images/frise_blog_post.svg"
import frise_home from "../images/frise_home.svg"
import frise_joinus from "../images/home_joinus_frise.svg"
import frise_opensource from "../images/frise_opensource.svg"
import frise_services from "../images/frise_services.svg"
import frise_use_cases from "../images/frise_use_cases.svg"

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
    /* Body fade-in
	 ========================================================================== */

    @-webkit-keyframes fadeIn {
      from {
        opacity: 0;
      }
      to {
        opacity: 1;
      }
    }
    @-moz-keyframes fadeIn {
      from {
        opacity: 0;
      }
      to {
        opacity: 1;
      }
    }
    @keyframes fadeIn {
      from {
        opacity: 0;
      }
      to {
        opacity: 1;
      }
    }

    /* Transitions
	 ========================================================================== */

    /*   for react-full-page */
    .transition--slide-fade-in {
      -webkit-transition: all 0.9s ease 0.3s;
      transition: all 0.9s ease 0.3s;
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
      -webkit-transition: all 0.9s ease 0.3s;
      transition: all 0.9s ease 0.3s;
      opacity: 0;
      visibility: hidden;

      &.delayed {
        transition-delay: 0.8s;
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
      &.button-small {
        padding: 7px 20px;
        font-size: 14px;
      }

      &.button-medium {
        padding: 8px 18px;
        font-size: 16px;
        line-height: 1.2;
      }

      &.button-medium-thin {
        padding: 5px 18px;
        font-size: 16px;
        line-height: 1.2;
      }

      &.button-secondary {
        border: 1px solid var(--fg-color);
        border-radius: 35px;
        text-align: center;
        color: var(--fg-color);
        background: linear-gradient(to right, black 50%, white 50%);
        background-size: 200% 100%;
        background-position: right bottom;
        transition: all 0.5s ease;

        &.inverted {
          border: 1px solid var(--fg-color);
          color: var(--fg-color);
          background: linear-gradient(to right, white 50%, black 50%);
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

    /* blog post styles.  */

    .blog-post-content {
      line-height: 28px;
      font-size: 18px;
      a,
      a:visited {
        color: ${t.colors.purple};
      }

      li {
        p {
          padding: 0px;
          width: 100%;
          margin: 0px !important;
        }
      }

      p {
        font-size: 18px;
        line-height: 1.5;
        padding: 0px 15px;
        margin-bottom: 20px;

        a,
        a:visited {
          color: ${t.colors.purple};
        }

        @media (min-width: ${t.breakpoints[1]}) {
          padding: 0px;
          padding-left: 60px;
          width: 65%;
          max-width: 1000px;
        }

        @media (min-width: ${t.breakpoints[5]}) {
          padding-left: 120px;
        }
      }

      code {
        background: var(--theme-ui-colors-greyLight, #f0f0f0);
        padding: 1px 8px;
        overflow: hidden;
        font-family: monospace;
        font-size: 1em;
        text-align: left;

        a {
          color: ${t.colors.orange} !important;
          &:visited {
            color: ${t.colors.orange} !important;
          }
        }
      }

      pre {
        code {
          display: block;
          line-height: 1.75;
          font-size: 16px;
          padding: 20px 55px;

          @media (min-width: ${t.breakpoints[1]}) {
            font-size: 18px;
          }
        }
      }

      h1 {
        padding: 0px;
        margin: 40px 15px 20px;
        font-size: 34px;
        max-width: 1000px;
        min-height: auto;

        @media (min-width: ${t.breakpoints[1]}) {
          margin: 0px;
          margin-bottom: 30px;
          padding: 0px;
          padding-left: 60px;
          width: 65%;
        }

        @media (min-width: ${t.breakpoints[5]}) {
          padding-left: 120px;
        }
      }

      h2 {
        padding: 0px;
        margin: 40px 15px 20px;
        width: 90%;
        font-size: 24px;
        line-height: 1;
        font-size: 700;
        text-transform: uppercase;

        @media (min-width: 768px) {
          padding: 0px;
          padding-left: 60px;
          margin: 60px 0px 30px;
          width: 65%;
          max-width: 1000px;
          font-size: 34px;
        }

        @media (min-width: ${t.breakpoints[5]}) {
          padding-left: 120px;
          font-size: 42px;
        }
      }

      h3 {
        margin: 20px 15px;
        padding: 0px 15px;
        width: 100%;
        max-width: 1000px;

        @media (min-width: ${t.breakpoints[1]}) {
          margin: 0px;
          margin-bottom: 30px;
          padding: 0px;
          padding-left: 60px;
          width: 65%;
        }
        @media (min-width: ${t.breakpoints[5]}) {
          padding-left: 120px;
        }
      }

      h4 {
        width: 65%;
        max-width: 1000px;
        margin: 0px 0px 30px;

        @media (min-width: ${t.breakpoints[1]}) {
          padding: 0 0 0 60px;
        }
        @media (min-width: ${t.breakpoints[5]}) {
          padding: 0 0 0 120px;
        }
      }

      ol {
        margin: 20px 15px;
        padding: 0px 15px;
        width: 100%;
        padding-left: 0px;

        list-style: none;
        counter-reset: ol-item-counter;

        @media (min-width: ${t.breakpoints[1]}) {
          margin: 0px 0px 30px;
          padding: 0 0 0 60px;
          max-width: 1000px;
          width: 65%;
        }

        @media (min-width: ${t.breakpoints[5]}) {
          padding: 0 0 0 120px;
        }

        li {
          position: relative;
          counter-increment: ol-item-counter;
          margin-left: 2em;

          &::before {
            content: counter(ol-item-counter) ". ";
            font-weight: bold;
            position: absolute;
            left: -2em;
            display: inline-block;
          }
        }
      }

      ul {
        margin: 30px 0px;
        padding: 0px 15px;
        max-width: 1000px;
        font-size: 18px;
        line-height: 1.5;

        @media (min-width: ${t.breakpoints[1]}) {
          margin: 0px;
          margin-bottom: 30px;
          padding: 0px;
          padding-left: 60px;
          width: 65%;
        }

        @media (min-width: ${t.breakpoints[5]}) {
          padding-left: 120px;
        }

        li {
          margin-bottom: 10px;
          padding-left: 35px;
          display: block;
          &::before {
            content: "";
            display: block;
            width: 8px;
            height: 8px;
            position: absolute;
            margin-left: -31px;
            margin-top: 5px;
            background: var(--fg-color);
            border-radius: 10px;
          }

          @media (min-width: ${t.breakpoints[1]}) {
            &::before {
              content: "";
              display: block;
              width: 10px;
              height: 10px;
              position: absolute;
              margin-left: -35px;
              margin-top: 12px;
              background: black;
              border-radius: 10px;
            }
          }
        }

        ul {
          display: block;
          padding-left: 0px;
          width: 100%;
        }

        ol {
          display: block;
          padding-left: 0px;
          width: 100%;
        }
      }

      .katex-display {
        margin: 20px 15px;
        padding: 0px 15px;
        width: 100%;

        @media (min-width: ${t.breakpoints[1]}) {
          padding: 0 0 0 60px;
          width: 65%;
          max-width: 1000px;
          margin: 0px 0px 30px;
        }

        @media (min-width: ${t.breakpoints[5]}) {
          padding: 0 0 0 120px;
        }
      }

      .twitter-tweet {
        margin: 0 0 20px 0 !important;
        padding: 0 15px;
        @media (min-width: ${t.breakpoints[1]}) {
          padding: 0 0 0 60px;
        }
        @media (min-width: ${t.breakpoints[5]}) {
          padding: 0 0 0 120px;
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
    .gm-style img {
      max-width: none;
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
    a:link.lined {
      text-decoration: underline;
      color: var(--fg-color);
    }
    .lined {
      display: inline;
      line-height: 28px;
      position: relative;
      -webkit-transition: all 0.4s ease;
      transition: all 0.4s ease;
      text-align: center;
      overflow: hidden;
      z-index: 2;
    }
    .lined:hover {
      color: var(--bg-color);
      background: var(--fg-color);
    }
    .lined:hover:after {
      width: 100%;
    }
    form,
    fieldset {
      margin: 0;
      padding: 0;
      border-style: none;
    }
    input[type="text"],
    input[type="tel"],
    input[type="email"],
    input[type="search"],
    input[type="password"],
    textarea {
      -webkit-appearance: none;
      -webkit-border-radius: 0;
      border-radius: 0;
      box-sizing: border-box;
      padding: 0.4em 0.7em;
    }
    input[type="text"]:focus,
    input[type="tel"]:focus,
    input[type="email"]:focus,
    input[type="search"]:focus,
    input[type="password"]:focus,
    textarea:focus {
      outline: none;
    }
    select {
      -webkit-border-radius: 0;
      border-radius: 0;
    }
    textarea {
      resize: vertical;
      vertical-align: top;
    }
    button,
    input[type="button"],
    input[type="reset"],
    input[type="file"],
    input[type="submit"] {
      -webkit-appearance: none;
      -webkit-border-radius: 0;
      border-radius: 0;
      /* cursor: pointer;  */
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
    .container {
      position: relative;
      margin: 0 auto;
      padding: 0 15px;
      max-width: 1440px;
    }

    .menu-active .nav-drop {
      background: var(--bg-color);
      color: var(--fg-color);
    }
    .home .nav-area {
      position: relative;
      padding-top: 0px;
    }
    .section-wrap {
      position: relative;
      padding: 130px 0;
      overflow: hidden;
    }
    .fp-section .container {
      max-width: 1440px;
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

    // In full-page mode (though not on small screens) we allow the content
    // to be seen behind the fixed nav header
    .fp-enabled body:not(.fp-responsive) header.s_white,
    .fp-enabled body:not(.fp-responsive) header.s_yellow,
    .fp-enabled body:not(.fp-responsive) header.s_purple,
    .fp-enabled body:not(.fp-responsive) header.s_blue,
    .fp-enabled body:not(.fp-responsive) header.s_black,
    .fp-enabled body:not(.fp-responsive) header.s_green,
    .fp-enabled body:not(.fp-responsive) header.s_beige,
    .fp-enabled body:not(.fp-responsive) header.s_grey,
    .fp-enabled body:not(.fp-responsive) header.s_red {
      background-color: transparent;
    }

    main > .fp-section:last-child .fp-tableCell {
      vertical-align: top;
      background: black; // Match the footer down to bottom of viewport
    }

    .section06 .fp-scroller {
      overflow: visible !important;
    }
    @keyframes scroll {
      to {
        background-position: -500px 0px; /* size of image */
      }
    }
    @keyframes scroll_r {
      to {
        background-position: -200px 0px; /* size of image */
      }
    }
    @keyframes scroll2 {
      to {
        background-position: -400px 0px; /* size of image */
      }
    }
    .backdrop {
      animation: 10s scroll infinite linear;
      background: url(${frise_joinus});
      min-height: 500px;
    }
    .backdrop2 {
      animation: 10s scroll2 infinite linear;
      background: url(${frise_home});
      background-size: 400px 200px;
      min-height: 200px;
    }
    .backdrop3 {
      animation: 10s scroll2 infinite linear;
      background: url(${frise_services});
      background-size: 400px 200px;
      min-height: 200px;
    }
    .backdrop4 {
      animation: 10s scroll2 infinite linear;
      background: url(${frise_biotech});
      background-size: 400px 200px;
      min-height: 200px;
    }
    .backdrop5 {
      animation: 10s scroll2 infinite linear;
      background: url(${frise_opensource});
      background-size: 400px 200px;
      min-height: 200px;
    }
    .backdrop6 {
      animation: 10s scroll2 infinite linear;
      background: url(${frise_blog_post});
      background-size: 400px 200px;
      min-height: 200px;
    }
    .backdrop7 {
      animation: 10s scroll2 infinite linear;
      background: url(${frise_blog_home});
      background-size: 400px 200px;
      min-height: 200px;
    }
    .backdrop8 {
      animation: 10s scroll2 infinite linear;
      background: url(${frise_use_cases});
      background-size: 400px 200px;
      min-height: 200px;
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
    .btn {
      display: inline-block;
      vertical-align: top;
      font-size: clamp(24px, 24px, 16px);
      line-height: 28px;
      position: relative;
      text-decoration: none;
      -webkit-transition: all 0.4s ease;
      transition: all 0.4s ease;
      border: 1px solid var(--fg-color);
      border-radius: 35px;
      text-align: center;
      height: 60px;
      padding: 15px 18px;
      color: var(--fg-color);
      background: transparent;
      overflow: hidden;
      z-index: 2;
      margin-bottom: 30px;
    }

    .btn:before {
      display: inline-block;
      vertical-align: top;
      font-family: "icomoon" !important;
      content: "\\e900";
      margin: 1px 4px 0 0;
    }
    .btn.noarrow {
      margin: -10px 15px;
    }
    .btn.noarrow:before {
      display: inline-block;
      vertical-align: top;
      font-family: "icomoon" !important;
      content: "";
      margin: 0;
    }
    .btn:after {
      content: "";
      position: absolute;
      left: 0;
      width: 0;
      top: 0;
      bottom: 0;
      -webkit-transition: all 0.4s ease;
      transition: all 0.4s ease;
      background: var(--fg-color);
      z-index: -1;
    }
    .btn:hover {
      color: var(--bg-color);
    }
    .btn:hover:after {
      width: 100%;
    }

    .header-inverse .black-logo,
    .s_purple .black-logo,
    .s_black.black-logo {
      filter: invert(100%);
    }
    .inner .logo {
      margin-top: 0;
    }
    .inner .logo,
    .inner .menu {
      transform: scale(1);
    }
    .line-sep {
      width: auto;
      margin: 40px 60px 0;
      height: 1px;
      background: var(--fg-color);
    }
    main nav ul li a {
      color: var(--fg-color);
    }
    main nav ul li a:hover {
      color: var(--fg-color);
      text-decoration: underline;
    }

    /* code section overrides */
    pre {
      background-color: #ecece9;
      font-size: ${t.fontSizes[2]};
      border-left: 15px solid ${t.colors.orange};
      margin-bottom: 30px;
      margin-top: 30px;
      padding-left: 50px;
      overflow: hidden;
      white-space: pre-wrap;
      word-wrap: break-word;
    }
    .operator,
    .constant,
    .namespace,
    .string {
      color: ${t.colors.orange};
    }
    .comment {
      color: #898a8c;
    }
    main nav {
      width: 65%;
      max-width: 1000px;
      margin: 0 0 30px;
      padding: 0 0 0 120px;
    }
    /**/
    /* *************************************************** */
    /* *************************************************** */
    /* *************************************************** */
    /* *************************************************** */
    /* ***************** End desktop ********************* */
    /* *************************************************** */
    /* *************************************************** */
    /* *************************************************** */
    /* *************************************************** */
    /* *************************************************** */

    @media (min-width: 768px) {
      .container {
        padding: 0px 100px;
      }

      .header.header-inverse {
        --fg-color: white;
        --bg-color: transparent;
      }
    }
    @media (min-width: 1024px) {
      .fp-viewing-0 .logo,
      .fp-viewing-0 .menu {
        margin-top: initial;
        -webkit-transform: scale(1);
        -ms-transform: scale(1);
        transform: scale(1);
      }
      .fp-viewing-0 .logo {
        padding-bottom: 17px;
      }
    }
    @media (min-width: 1400px) {
      .image-holder img {
        width: 100%;
        height: 100%;
      }
      h2 {
        font-size: ${t.fontSizes[6]};
      }
      h3 {
        font-size: ${t.fontSizes[5]};
      }
    }
    @media (min-width: 1601px) {
      .fp-section .container {
        max-width: 100%;
      }
    }
    @media (max-width: 1499px) {
      main nav {
        padding: 0 0 0 60px;
      }
      pre {
        padding-left: 50px;
      }
      .container {
        padding: 0 60px;
      }
      .menu > li {
        position: relative;
        margin: 0 15px;
      }
      .section-wrap {
        padding: 100px 0;
      }
      h2 {
        font-size: ${t.fontSizes[5]};
      }
      h3 {
        font-size: ${t.fontSizes[4]};
      }
      .btn {
        font-size: ${t.fontSizes[2]};
        line-height: 1.2;
        height: 44px;
        padding: 9px 18px;
      }
    }
    @media (max-width: 767px) {
      code,
      kbd,
      pre,
      samp {
        font-family: monospace;
        font-size: ${t.fontSizes[1]};
        text-align: left;
      }
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
      header {
        background: var(--bg-color);
      }
      main nav {
        margin: 30px 15px !important;
        padding: 0;
      }
      pre {
        padding-left: 0;
        overflow: hidden;
        white-space: pre-wrap;
        word-wrap: break-word;
        padding-left: 0 !important;
        margin: 30px 0;
      }

      .backdrop {
        animation: 8s scroll_r infinite linear;
        background-size: contain;
        min-height: 200px;
      }
      .btn {
        font-size: ${t.fontSizes[1]};
        margin: 0;
      }
      .btn {
        font-size: ${t.fontSizes[2]};
        padding: 8px 18px;
      }
      .container {
        padding: 0;
      }
      .line-sep {
        margin: 20px;
      }
      .menu-active .nav-area {
        -webkit-transform: translateY(-10px);
        -ms-transform: translateY(-10px);
        transform: translateY(-10px);
      }
      .menu-active .nav-drop {
        max-height: 4000px;
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
      .section-wrap {
        padding: 0;
        margin-bottom: 0;
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

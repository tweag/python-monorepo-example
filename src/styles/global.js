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
    .contact .line-sep {
      padding: 0;
      margin: 20px 60px 40px;
    }
    .contact-addr .description {
      text-align: left;
    }
    .contact-addr .container {
      margin-left: 0;
      padding-left: 120px;
    }
    .contact-addr .col-area .description {
      margin: 0;
    }
    .contact-addr .col-area .info-col {
      width: auto;
      padding-right: 80px;
    }
    .contact-addr.section-wrap {
      padding: 20px 0 60px 0;
    }
    .contact-addr h3 {
      font-size: ${t.fontSizes[8]};
    }
    .contact-field label,
    .contact-addr label {
      font-weight: bold;
    }
    .contact-field.section-area {
      padding: 60px 0 0 0;
    }
    .contact-addr .hello {
      font-size: ${t.fontSizes[5]};
      line-height: 1.5;
      padding-bottom: 50px;
    }
    .contact-field input,
    .contact-field textarea {
      display: block;
      margin: 20px 0;
      border: 0;
      background: transparent;
      padding: 20px 0 40px 0 !important;
      font-size: ${t.fontSizes[6]};
      font-weight: normal;
      width: 100%;
    }
    .contact-field .text-area {
      font-size: ${t.fontSizes[3]};
      line-height: 35px;
    }
    .contact-field .btn {
      font-size: ${t.fontSizes[3]};
    }
    .contact-field textarea::placeholder {
      opacity: 0.3;
    }
    .contact-field textarea {
      min-height: 300px;
    }
    .contact-field input::placeholder {
      opacity: 0.3;
      text-indent: 0;
    }
    .contact-field {
      padding: 60px 0 0 0;
    }
    .fade-in {
      opacity: 0; /* make things invisible upon start */
      -webkit-animation: fadeIn ease-in 1; /* call our keyframe named fadeIn, use animattion ease-in and repeat it only 1 time */
      -moz-animation: fadeIn ease-in 1;
      animation: fadeIn ease-in 1;
      -webkit-animation-fill-mode: forwards; /* this makes sure that after animation is done we remain at the last keyframe value (opacity: 1)*/
      -moz-animation-fill-mode: forwards;
      animation-fill-mode: forwards;
      -webkit-animation-duration: 2s;
      -moz-animation-duration: 2s;
      animation-duration: 2s;
    }
    .reverse {
      flex-direction: row-reverse;
    }
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
    .line-arrow {
      display: none;
      position: fixed;
      margin: -30px 50%;
      width: 40px;
      height: 40px;
    }
    .line-arrow.down {
      border-top: 1px solid var(--fg-color);
      border-right: 1px solid var(--fg-color);
      transform: rotate(135deg) skew(0deg);
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
    .vision-area .container {
      max-width: 100%;
      padding: 0 0 0 120px;
    }
    .sticky-wrap-header {
      height: 1px;
      -webkit-transition: all 0.8s ease-out;
      -moz-transition: all 0.8s ease-out;
      -o-transition: all 0.8s ease-out;
      -ms-transition: all 0.8s ease-out;
      transition: all 0.8s ease-out;
    }
    .menu-active .menu-opener {
      border-top: none;
    }
    .menu-active .menu-opener:before,
    .menu-active .menu-opener:after {
      -webkit-transform: rotate(45deg);
      -ms-transform: rotate(45deg);
      transform: rotate(45deg);
      top: 10px;
    }
    .menu-active .menu-opener:after {
      -webkit-transform: rotate(-45deg);
      -ms-transform: rotate(-45deg);
      transform: rotate(-45deg);
      top: 10px;
    }
    .home .nav-area {
      position: relative;
      padding-top: 0px;
    }
    .fixed-header .nav-area {
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
    .section-wrap.key_indus {
      position: relative;
      padding: 130px 50px;
      overflow: hidden;
    }
    .section-wrap.biotech {
      position: relative;
      padding: 100px 0;
      overflow: hidden;
    }
    .section {
      position: relative;
    }
    .section.active .visual-holder .image-holder,
    .section.active .visual-holder .visual-caption,
    .section.active .contactus-holder .image-col,
    .section.active .contactus-holder .text-area,
    .section.active .col-area h2,
    .section.active .col-area .info-col,
    .section.active .joinus-holder .text-wrap,
    .section.active .joinus-holder .image-holder,
    .section.active .vision-holder .text-col,
    .section.active .vision-holder .image-holder,
    .section.active .partners-area h2,
    .section.active .partners-list li {
      -webkit-transform: none;
      -ms-transform: none;
      transform: none;
      opacity: 1;
      visibility: visible;
    }
    @media only percy {
      .section .visual-holder .image-holder,
      .section .visual-holder .visual-caption,
      .section .contactus-holder .image-col,
      .section .contactus-holder .text-area,
      .section .col-area h2,
      .section .col-area .info-col,
      .section .joinus-holder .text-wrap,
      .section .joinus-holder .image-holder,
      .section .vision-holder .text-col,
      .section .vision-holder .image-holder,
      .section .partners-area h2,
      .section .partners-list li {
        transform: none;
        opacity: 1;
        visibility: visible;
      }
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
    .visual-area {
      position: relative;
    }
    .visual-holder {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: start;
      -ms-flex-align: start;
      align-items: flex-start;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
      padding: 30px 0 0;
    }
    .visual-holder .image-holder {
      position: relative;
      width: 40%;
    }
    .visual-holder .image-holder video {
      display: block;
      width: 100%;
      height: auto;
    }
    .visual-holder .visual-caption {
      font-size: ${t.fontSizes[4]};
      line-height: 35px;
      position: relative;
      width: 40%;
    }
    .visual-holder h1 {
      margin: 0 0 15px;
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
    .contactus-area {
      position: relative;
    }
    .contactus-holder {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: start;
      -ms-flex-align: start;
      align-items: flex-start;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
    }
    .contactus-holder .image-col {
      position: relative;
      width: 40%;
    }
    .contactus-holder h2 {
      max-width: 400px;
      margin: 0 0 50px;
    }
    .contactus-holder .image-holder {
      position: relative;
    }
    .contactus-holder .image-holder img {
      display: block;
      width: 100%;
      height: auto;
    }
    .contactus-holder .text-area {
      position: relative;
      width: 40%;
    }
    .contactus-holder .text-list {
      font-size: ${t.fontSizes[2]};
      line-height: 26px;
      margin: 0;
      padding: 0;
      list-style: none;
      position: relative;
      margin: 0 0 70px;
    }
    .contactus-holder .text-list li {
      position: relative;
      margin: 0 0 40px;
    }
    .contactus-holder .text-list h3 {
      margin: 0 0 18px;
    }
    .contactus-holder .text-list p {
      margin: 0;
    }
    .col-area {
      position: relative;
    }
    .col-area h2 {
      text-align: center;
      margin: 0 0 60px;
    }
    .col-area .col-row {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: start;
      -ms-flex-align: start;
      align-items: flex-start;
      margin: 0 -15px;
    }
    .col-area .info-col {
      position: relative;
      width: 33.333%;
      padding: 0 15px;
      margin: 0 0 30px;
    }
    .col-area .info-col2,
    .col-area .info-col1 {
      position: relative;
      width: 50%;
      padding: 0 15px;
      margin: 0 0 30px;
    }
    .col-area .info-col3 {
      position: relative;
      width: auto;
      padding: 0 15px;
      margin: 0 0 30px;
    }
    .col-area .info-col2 .description,
    .col-area .info-col1 .description {
      max-width: 400px;
      margin: auto;
    }
    .col-area .block {
      position: relative;
      text-align: center;
    }
    .col-area .image-holder {
      position: relative;
      width: 250px;
      margin: 0 auto 60px;
    }
    .col-area .image-holder img {
      display: block;
      width: 70%;
      height: auto;
      margin: auto;
    }
    .col-area .description {
      font-size: ${t.fontSizes[2]};
      line-height: 26px;
      position: relative;
    }
    .col-area .description h3 {
      margin: 0 0 18px;
    }
    .col-area .description p {
      margin: 0 0 50px;
    }
    .joinus-area {
      position: relative;
    }
    .joinus-holder {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
      margin-right: calc((100vw - 1410px) / -2);
    }
    .joinus-holder .text-wrap {
      position: relative;
      width: 40%;
    }
    .joinus-holder .title {
      font-size: ${t.fontSizes[6]};
      line-height: 50px;
      font-weight: 700;
      display: block;
      margin: 0 0 40px;
    }
    .joinus-holder .image-holder {
      position: relative;
      width: 45%;
    }
    .joinus-holder .image-wrap img {
      display: block;
      width: 100%;
      height: auto;
    }
    .partners-area {
      position: relative;
    }
    .partners-area h2 {
      text-align: center;
      margin: 0 0 60px;
    }
    .partners-list {
      margin: 0;
      padding: 0;
      list-style: none;
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: center;
      -ms-flex-align: center;
      align-items: center;
    }
    .partners-area2 {
      padding: 100px;
    }
    .partners-list li {
      position: relative;
      width: 33.3333%;
      margin: 0 0 60px;
    }
    .partners-list .logo-wrap {
      position: relative;
      text-align: center;
    }
    .partners-list .logo-wrap img {
      display: inline-block;
      vertical-align: middle;
      position: relative;
    }
    .vision-area {
      position: relative;
      width: 100%;
    }
    .vision-holder {
      padding: 60px 0 60px 0px;
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: center;
      -ms-flex-align: center;
      align-items: center;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
      margin-right: calc((100vw - 1410px) / -2);
    }
    .vision-holder .image-holder {
      position: relative;
      width: 45%;
    }
    .vision-holder .image-wrap {
      position: relative;
    }
    .vision-holder .image-wrap img {
      display: block;
      width: 100%;
      height: auto;
    }
    .vision-holder .text-col {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: end;
      -ms-flex-align: end;
      align-items: flex-end;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
      width: 33%;
    }
    .vision-holder h2 {
      max-width: 400px;
      line-height: 1.3;
      padding-bottom: 20px;
    }
    .about-section .image-holder img {
      width: 100%;
      height: auto;
      display: block;
    }
    .about-section .image-holder {
      position: relative;
      width: 35%;
    }
    .about-section .text-area {
      position: relative;
      padding: 0 0 0 50px;
    }
    .about-section .text-list li i {
      display: inline-block;
      vertical-align: top;
      margin: 4px 6px 0 0;
    }
    .about-section .text-list li p {
      font-size: ${t.fontSizes[3]};
      font-weight: normal;
      display: inline-block;
      vertical-align: top;
      margin: 20px 6px;
    }
    .about-section.biotech {
      padding: 60px 0 100px 0;
    }
    .about-section.opensource {
      padding: 20px 0 40px 0;
      align-items: start;
      justify-content: normal;
      font-size: ${t.fontSizes[4]};
    }
    .about-section.opensource2 {
      padding: 20px 0 40px 0;
    }
    .about-section.opensource3 {
      padding: 20px 0 40px 0;
    }
    .about-section.opensource4 {
      padding: 40px 0 60px 0;
    }
    .about-section.opensource4 p.sample {
      font-weight: bold;
      padding: 20px 0 40px 0;
      max-width: 500px;
    }
    .about-section.learnmore .text-area {
      padding: 50px 220px;
      width: 60%;
    }
    .about-section.learnmore h3 {
      padding-bottom: 20px;
    }
    .about-section.learnmore .split {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: center;
      -ms-flex-align: center;
      align-items: center;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
      margin-bottom: 60px;
      font-size: ${t.fontSizes[2]};
    }
    .about-section.learnmore .split-col1 {
      width: 40%;
    }
    .about-section.learnmore .split-col2 {
      width: 40%;
    }
    .about-section.learnmore .btn {
      margin-top: 20px;
      height: auto;
    }
    .about-section.learnmore .btn:hover {
      margin-top: 20px;
    }
    .about-section.learnmore h3.quote {
      padding-top: 80px;
    }
    .about-section.learnmore h3.quote::after {
      content: "";
      display: block;
      width: 100px;
      height: 1px;
      border-bottom: 1px solid black;
      padding-top: 40px;
    }
    .about-section.biotech .text-list li p {
      font-size: ${t.fontSizes[4]};
      font-weight: normal;
      display: block;
      margin: 20px 6px;
      padding-left: 20px;
    }
    .about-section.biotech .icon-arrow-right1 {
      float: left;
    }
    .about-section .text-list li {
      position: relative;
      margin: 0 0 15px;
    }
    .about-section .text-list {
      position: relative;
      list-style: none;
      padding: 0;
      margin: 50px 0;
      font-size: ${t.fontSizes[5]};
      line-height: 1.1;
      font-weight: 700;
    }
    .about-section {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: center;
      -ms-flex-align: center;
      align-items: center;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
    }
    .copyright {
      position: relative;
      display: block;
    }
    .copyright-area {
      font-size: ${t.fontSizes[0]};
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: center;
      -ms-flex-align: center;
      align-items: center;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
      margin: 50px 0px 25px;
    }
    .copyright-col1 {
      position: relative;
      width: 80%;
      margin: 0 0 22px;
    }
    .copyright-col2 {
      position: relative;
      width: 20%;
      margin: 0 0 22px;
    }
    .f-logo a {
      display: block;
    }
    .f-logo img {
      display: block;
      width: 100%;
      height: auto;
    }
    .f-logo {
      position: relative;
      display: block;
      width: 152px;
    }
    .header-inverse .black-logo,
    .s_purple .black-logo,
    .s_black.black-logo {
      filter: invert(100%);
    }
    .header-inverse .menu-opener {
      border-top-color: white;
    }
    .header-inverse .menu-opener:after,
    .header-inverse .menu-opener:before {
      background: white;
    }
    .in-viewport .col-area .info-col,
    .in-viewport .col-area h2,
    .in-viewport .vision-holder .text-col,
    .in-viewport .vision-holder .image-holder,
    .in-viewport .partners-area h2,
    .in-viewport .partners-list li {
      opacity: 1;
      visibility: visible;
      -webkit-transform: none;
      -ms-transform: none;
      transform: none;
    }
    .inner .fixed-header .logo {
      margin-top: -14px;
    }
    .inner .fixed-header .logo,
    .inner .fixed-header .menu {
      transform: scale(0.9);
    }
    .inner .logo {
      margin-top: 0;
    }
    .inner .logo,
    .inner .menu {
      transform: scale(1);
    }
    .mail-area .mail {
      display: block;
    }
    .mail-area .mail-title {
      font-weight: 300;
      position: relative;
      display: block;
      margin: 0 0 20px;
      font-size: ${t.fontSizes[3]};
      line-height: 24px;
    }
    .mail-area {
      position: relative;
    }
    .method-area .image-holder img {
      width: 100%;
      height: auto;
      display: block;
    }
    .method-area .image-holder {
      position: relative;
      width: 24%;
    }
    .method-area .text-area .text i {
      display: inline-block;
      vertical-align: top;
      margin: 1px 2px 0 0;
    }
    .method-area .text-area .text {
      font-size: ${t.fontSizes[2]};
      line-height: 17px;
      display: block;
    }
    .method-area .text-area h2 .icon-arrow-right1 {
      display: block;
      margin: 0 0 20px;
    }
    .method-area .text-area h2 .icon-arrow-right1:before {
      -webkit-transform: rotate(90deg);
      -ms-transform: rotate(90deg);
      transform: rotate(90deg);
      display: inline-block;
      vertical-align: top;
    }
    .method-area .text-area h2 {
      font-size: ${t.fontSizes[5]};
      line-height: 1.1;
      text-transform: none;
      margin: 0 0 20px;
    }
    .method-area .text-area h2::after {
      content: "";
      display: block;
      width: 100px;
      height: 1px;
      border-bottom: 1px solid var(--fg-color);
      padding-top: 40px;
    }
    .method-area .text-area {
      padding: 0 0 0 180px;
      width: 50%;
    }
    .method-area {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: center;
      -ms-flex-align: center;
      align-items: center;
      margin: 0 0 30px;
    }
    .section-area *::-moz-selection {
      background: rgba(0, 0, 0, 0.99); /* Gecko Browsers */
      color: white;
    }
    .section-area *::selection {
      background: rgba(0, 0, 0, 0.99); /* WebKit/Blink Browsers */
      color: white;
    }
    .section-area.services {
      padding: 0 auto 60px;
    }
    .section-area {
      position: relative;
      padding: 160px 0 0px;
    }
    .section-title {
      position: relative;
      border-bottom: 1px solid var(--fg-color);
      color: var(--fg-color);
      text-align: center;
      padding: 0px 0px 10px 0;
      font-size: ${t.fontSizes[2]};
      line-height: 22px;
      font-weight: 400;
      border-radius: 0;
      display: inline-block;
      vertical-align: top;
      overflow: hidden;
      z-index: 2;
      cursor: default;
    }
    .section-title.homepost {
      border: 0px;
      padding-top: 15px;
      font-size: ${t.fontSizes[0]};
    }
    .services-section .image-holder img {
      width: 100%;
      height: auto;
      display: block;
    }
    .services-section .image-holder {
      position: relative;
      width: 20%;
    }
    .services-section .text-area h1 .icon-arrow-right1 {
      font-size: 100%;
      line-height: 1;
    }
    .services-section.blog-content.topsep {
      margin-bottom: 60px;
    }
    .services-section.blog-content.topsep > .text-area {
      margin: 40px 0 0 120px;
      border-top: 1px solid var(--fg-color);
      padding-left: 0px;
    }
    .services-section .text-area h1 {
      margin: 50px 0 35px;
    }
    .services-section.topsep .text-area h1 {
      margin: 20px 0 35px;
    }
    .services-section.topsep .text-area p {
      font-size: ${t.fontSizes[4]};
    }
    .services-section .text-area h3 {
      margin: 50px 0 0 0;
    }
    .services-section .text-area {
      position: relative;
      width: 49%;
      padding: 0 0 0 120px;
    }
    .services-section.blog-home > .text-area {
      position: relative;
      width: 43%;
      padding: 0 0 0 120px;
    }
    .services-section.opensource1 .text-area {
      margin: 0 0 30px;
    }
    .services-section.opensource1.part2 .text-area {
      margin: 0;
      padding: 0;
    }
    .services-section .text-wrap p {
      margin: 0 0 15px;
    }
    .services-section .text-wrap {
      position: relative;
      font-size: ${t.fontSizes[4]};
      line-height: 35px;
      max-width: 800px;
    }
    .services-section.blog-content .text-wrap {
      font-size: ${t.fontSizes[2]};
      line-height: 28px;
    }
    .services-section {
      position: relative;
      display: -webkit-box;
      display: -ms-flexbox;
      display: flex;
      -ms-flex-wrap: wrap;
      flex-wrap: wrap;
      -webkit-box-align: start;
      -ms-flex-align: start;
      align-items: flex-start;
      -webkit-box-pack: justify;
      -ms-flex-pack: justify;
      justify-content: space-between;
    }
    .biotech .image-holder {
      width: 50%;
      margin: 0;
    }
    .biotech .text-area .text-wrap {
      max-width: 800px;
      margin: 50px 0;
    }
    .biotech .text-area h1 {
      margin: 30px 0;
      width: 80%;
    }
    .biotech .text-area,
    .opensource .text-area {
      position: relative;
      width: 100%;
      padding: 0 0 0 120px;
    }
    .biotech .text-area2,
    .opensource .text-area2 {
      position: relative;
      width: 50%;
      padding: 80px 0 0 120px;
      margin: 0 auto;
      max-width: 800px;
    }
    .about-section.biotech .text-list {
      font-size: ${t.fontSizes[4]};
    }
    .line-sep {
      width: auto;
      margin: 40px 60px 0;
      height: 1px;
      background: var(--fg-color);
    }
    /* Blog post */

    .blog-content h1 a,
    .blog-content h2 a,
    .blog-content h1 a:visited,
    .blog-content h2 a:visited,
    .blog-content h1 a:active,
    .blog-content h2 a:active {
      color: var(--fg-color);
      text-decoration: none;
    }
    .blog-content h1 a:hover,
    .blog-content h2 a:hover {
      text-decoration: underline;
    }
    .blog-content .article-text a,
    .blog-content .article-text a:visited {
      color: ${t.colors.purple};
    }
    .services-section .posts-holder.image-holder {
      margin-top: 0;
      width: 40%;
    }
    .post_container {
      margin: 0 120px;
      width: 100%;
      display: flex;
      justify-content: space-between;
      flex-wrap: wrap;
    }
    .post_content {
      width: 30%;
      border-top: 1px solid var(--fg-color);
      background: var(--bg-color);
      margin-bottom: 40px;
      padding: 0;
      display: flex;
      flex-direction: column;
      height: 350px;
      justify-content: space-between;
    }
    .post_content.notop {
      border-top: 0px solid var(--fg-color);
    }
    .post_date {
      padding-top: 15px;
      padding-bottom: 10px;
    }
    .post_title,
    .post_excerpt {
      padding-bottom: 20px;
    }
    .post_excerpt p {
      display: -webkit-box;
      -webkit-line-clamp: 4;
      -webkit-box-orient: vertical;
      overflow: hidden;
    }
    .blog-content .article-text h2 {
      margin: 80px 0 40px 0;
    }
    .blog-content .post_title h2 {
      display: -webkit-box;
      -webkit-line-clamp: 3;
      -webkit-box-orient: vertical;
      overflow: hidden;
    }
    .blog-content .article-text ul li,
    .blog-content .article-text ol {
      display: block;
      padding-left: 35px;
    }
    /* Nested lists */
    .blog-content .article-text ul li ul,
    .blog-content .article-text ul li ol {
      display: block;
      padding-left: 0px;
      width: 100%;
    }
    .blog-content .article-text ul li::before {
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
    .blog-content .article-text p {
      margin: 0 0 20px 0 !important;
    }
    .blog-content .article-text li p {
      padding: 0;
      margin: 0 !important;
      width: 100%;
    }
    .blog-content .article-text a,
    .blog-content .article-text a:visited {
      color: ${t.colors.purple};
    }
    .blog-content .article-text code a,
    .blog-content .article-text code a:visited {
      color: ${t.colors.orange} !important;
    }
    .blog-content .article-text code {
      background: ${t.colors.greyLight};
      padding: 1px 8px;
      overflow: hidden;
    }
    .blog-content .article-text pre code {
      display: block;
      width: 100%;
      padding: 20px 55px;
      color: var(--fg-color);
      background: #ecece9;
      overflow: hidden;
    }
    /* Code inside lists */
    .blog-content .article-text li pre code {
      padding: 20px 0px;
    }
    main nav ul li a {
      color: var(--fg-color);
    }
    main nav ul li a:hover {
      color: var(--fg-color);
      text-decoration: underline;
    }
    .code-section pre {
      padding: 40px 0;
      font-size: ${t.fontSizes[2]};
      overflow: hidden;
    }
    .blog-content a.btn.noarrow {
      display: inline-block;
      vertical-align: top;
      font-size: ${t.fontSizes[1]};
      line-height: 28px;
      position: relative;
      text-decoration: none;
      -webkit-transition: all 0.4s ease;
      transition: all 0.4s ease;
      border: 1px solid var(--fg-color);
      border-radius: 35px;
      text-align: center;
      height: 30px;
      padding: 0px 20px;
      color: var(--fg-color);
      overflow: hidden;
      z-index: 2;
      margin: 0px 15px 5px 0;
      text-transform: none;
      font-weight: normal;
    }
    .blog-content a.btn.noarrow:hover {
      color: var(--bg-color);
    }
    /* code section overrides */

    .part2 .text-area {
      margin: 0;
      padding: 0;
      width: 100%;
    }
    .services-section .text-wrap {
      max-width: 100%;
    }
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
    .part2 .text-area p,
    .part2 .text-area h1,
    .part2 .text-area h2,
    .part2 .text-area h3,
    .part2 .text-area h4,
    .part2 .text-area ul,
    .part2 .text-area ol,
    .part2 .text-area center,
    .part2 .text-area .katex-display,
    main nav {
      width: 65%;
      margin: 0 0 30px;
      padding: 0 0 0 120px;
    }
    .part2 .text-area h1 {
      font-size: ${t.fontSizes[5]};
    }
    .part2 .text-area h2 {
      margin: 60px 0 30px;
      padding: 0 0 0 120px;
      overflow: hidden;
      display: -webkit-box;
      -webkit-line-clamp: 3;
      -webkit-box-orient: vertical;
    }
    .services-section .posts-holder.image-holder {
      display: none;
    }
    /**/

    p.use-case-excerpt {
      font-size: ${t.fontSizes[5]};
      font-weight: bold;
      padding: 10px 0 30px 0;
    }
    img.uc-rotate {
      transform: rotate(180deg);
    }
    .use_case .image-holder {
      margin-right: -8.2%;
      width: 27%;
      margin-top: 50px;
    }
    .use_case.bloc1 {
      padding: 80px 0;
    }
    .use_case.bloc1 .text-area h1,
    .use_case.bloc2 .text-area h1 {
      text-align: center;
    }
    .use_case.bloc1 .text-area p,
    .use_case.bloc2 .text-area p {
      margin-top: 60px;
      text-align: left;
      font-size: ${t.fontSizes[4]};
    }
    .use_case.bloc1 .text-area,
    .use_case.bloc2 .text-area {
      width: 800px;
      padding: 0;
      margin: auto;
    }
    .use_case.bloc2 {
      padding: 80px 0;
    }
    .services-section.use-cases .image-holder.image-holder2 {
      width: 17%;
      margin-top: 0;
    }
    .services-section.use-cases.second {
      margin: 100px 0;
    }
    .services-section.use-cases.second .text-area2 {
      margin-right: 14%;
      max-width: 800px;
    }
    .use_case.results {
      padding: 80px 0;
      margin: 0;
    }
    .use_case.results h1 {
      padding: 0;
      margin: 0;
    }
    .use_case.results ul {
      margin-top: 100px;
    }
    .use_case.results li {
      margin-bottom: 10px;
      font-weight: bold;
      list-style: none;
    }
    .use_case.results li:before {
      content: "→";
      display: block;
      position: absolute;
      margin-left: -35px;
    }
    .use_case.results quote {
      display: block;
      margin-top: 80px;
      max-width: 700px;
    }
    .use_case.results quote h3 {
      display: block;
      margin-top: 80px;
    }
    .use_case.results quote h3::after {
      content: "";
      display: block;
      width: 100px;
      height: 1px;
      border-bottom: 1px solid var(--fg-color);
      padding-top: 40px;
      margin-bottom: 20px;
    }
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
      h1.typeit {
        min-height: 200px;
      }
      .col-area .image-holder {
        width: 220px;
        margin: auto;
      }
      .section-wrap.head1 {
        padding: 60px;
      }
      .container {
        padding: 0px 100px;
      }
      .joinus-area .container {
        padding: 0 0 0 60px;
      }
      .contactus-area .container {
        padding: 0 60px;
      }
      .vision-area .container {
        padding-left: 60px;
        max-width: 100%;
      }
      .vision-holder {
        padding: 60px 0 60px 0px;
      }
      .about-section.learnmore .text-area {
        padding: 20px;
      }
      .visual-holder .image-holder {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .visual-holder .visual-caption {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
        -webkit-transition-delay: 0.8s;
        transition-delay: 0.8s;
      }
      .contactus-holder .image-col {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .contactus-holder .text-area {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
        -webkit-transition-delay: 0.5s;
        transition-delay: 0.5s;
      }
      .col-area h2 {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .col-area .info-col {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .col-area .info-col:nth-child(1) {
        -webkit-transition-delay: 0.3s;
        transition-delay: 0.3s;
      }
      .col-area .info-col:nth-child(2) {
        -webkit-transition-delay: 0.6s;
        transition-delay: 0.6s;
      }
      .col-area .info-col:nth-child(3) {
        -webkit-transition-delay: 0.9s;
        transition-delay: 0.9s;
      }
      .joinus-holder .text-wrap {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .joinus-holder .image-holder {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
        -webkit-transition-delay: 0.9s;
        transition-delay: 0.9s;
      }
      .partners-area h2 {
        -webkit-transition: all 0.4s ease 0.3s;
        transition: all 0.4s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .partners-list li {
        -webkit-transition: all 0.4s ease 0.3s;
        transition: all 0.4s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .partners-list li:nth-child(1) {
        -webkit-transition-delay: 0.3s;
        transition-delay: 0.3s;
      }
      .partners-list li:nth-child(2) {
        -webkit-transition-delay: 0.5s;
        transition-delay: 0.5s;
      }
      .partners-list li:nth-child(3) {
        -webkit-transition-delay: 0.7s;
        transition-delay: 0.7s;
      }
      .partners-list li:nth-child(4) {
        -webkit-transition-delay: 0.9s;
        transition-delay: 0.9s;
      }
      .partners-list li:nth-child(5) {
        -webkit-transition-delay: 1.1s;
        transition-delay: 1.1s;
      }
      .partners-list li:nth-child(6) {
        -webkit-transition-delay: 1.3s;
        transition-delay: 1.3s;
      }
      .partners-list li:nth-child(7) {
        -webkit-transition-delay: 1.5s;
        transition-delay: 1.5s;
      }
      .partners-list li:nth-child(8) {
        -webkit-transition-delay: 1.7s;
        transition-delay: 1.7s;
      }
      .vision-holder .image-holder {
        -webkit-transition: all 1.9s ease 0.3s;
        transition: all 1.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
        -webkit-transition-delay: 0.9s;
        transition-delay: 0.9s;
      }
      .vision-holder .text-col {
        -webkit-transition: all 0.9s ease 0.3s;
        transition: all 0.9s ease 0.3s;
        opacity: 0;
        visibility: hidden;
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
      }
      .header.header-inverse {
        --fg-color: white;
        --bg-color: transparent;
      }
      .services-section .text-area {
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
      }
      .services-section .text-area2 {
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        -webkit-transition-delay: 0s;
        transition-delay: 0s;
        opacity: 0;
        visibility: hidden;
      }
      .services-section .image-holder {
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
        margin-top: 200px;
      }
      .home-holder.image-holder,
      .posts-holder.image-holder {
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
        width: 40%;
        font-size: ${t.fontSizes[2]};
        padding-right: 120px;
      }
      .home-holder.image-holder {
        margin-top: 40px;
      }
      .posts-holder.image-holder ul {
        padding: 0;
      }
      .posts-holder.image-holder li {
        margin: 20px 0;
        padding: 0 0 20px;
        list-style: none;
        border-bottom: 1px solid black;
      }
      .posts-holder.image-holder h3 {
        border-bottom: 1px solid black;
        padding: 0 0 15px 0;
        margin-bottom: 20px;
      }
      .services-section .image-holder2 {
        -webkit-transform: translateX(-300px);
        -ms-transform: translateX(-300px);
        transform: translateX(-300px);
      }
      .services-section .image-holder3 {
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
      }
      .biotech .image-holder,
      .opensource .image-holder {
        width: 40%;
        margin: 50px auto;
      }
      .biotech .image-holder2,
      .opensource .image-holder2 {
        width: 24%;
        margin: 50px 0 80px 0;
      }
      .biotech .image-holder3,
      .opensource .image-holder3 {
        width: 24%;
        margin: 80px 0 80px 0;
      }
      .services-section .image-holder {
        -webkit-transition-delay: 0.3s;
        transition-delay: 0.3s;
      }
      .services-section .image-holder2 {
        -webkit-transition-delay: 0s;
        transition-delay: 0s;
      }
      .services-section .image-holder3 {
        -webkit-transition-delay: 0s;
        transition-delay: 0s;
      }
      .services-section.in-viewport .text-area,
      .services-section.in-viewport .image-holder,
      .services-section.in-viewport .text-area2 {
        opacity: 1;
        visibility: visible;
        -webkit-transform: none;
        -ms-transform: none;
        transform: none;
      }
      @media only percy {
        .services-section .text-area,
        .services-section .image-holder,
        .services-section .text-area2 {
          opacity: 1;
          visibility: visible;
          transform: none;
        }
      }
      .method-area .text-area {
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
      }
      .method-area .image-holder {
        -webkit-transform: translateX(-300px);
        -ms-transform: translateX(-300px);
        transform: translateX(-300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
      }
      .method-area .text-area {
        -webkit-transition-delay: 0.3s;
        transition-delay: 0.3s;
      }
      .method-area.in-viewport .text-area,
      .method-area.in-viewport .image-holder {
        opacity: 1;
        visibility: visible;
        -webkit-transform: none;
        -ms-transform: none;
        transform: none;
      }
      @media only percy {
        .method-area .text-area,
        .method-area .image-holder {
          opacity: 1;
          visibility: visible;
          transform: none;
        }
      }
      .about-section .text-area {
        -webkit-transform: translateY(300px);
        -ms-transform: translateY(300px);
        transform: translateY(300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
        width: 50%;
        margin-top: 50px;
      }
      .about-section .image-holder {
        -webkit-transform: translateX(300px);
        -ms-transform: translateX(300px);
        transform: translateX(300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
      }
      .about-section .image-holder4 {
        -webkit-transform: translateX(-300px);
        -ms-transform: translateX(-300px);
        transform: translateX(-300px);
        -webkit-transition: all 0.9s ease;
        transition: all 0.9s ease;
        opacity: 0;
        visibility: hidden;
      }
      .about-section .image-holder4 img {
        width: 70%;
      }
      .about-section .image-holder {
        -webkit-transition-delay: 0.3s;
        transition-delay: 0.3s;
      }
      .about-section.in-viewport .text-area,
      .about-section.in-viewport .image-holder {
        opacity: 1;
        visibility: visible;
        -webkit-transform: none;
        -ms-transform: none;
        transform: none;
      }
      @media only percy {
        .about-section .text-area,
        .about-section .image-holder {
          opacity: 1;
          visibility: visible;
          transform: none;
        }
      }
      .w30 {
        width: 30% !important;
      }
      .w70 {
        width: 50% !important;
      }
      .w100 {
        width: 100% !important;
      }
    }
    @media (min-width: 1024px) {
      .line-arrow {
        display: block;
      }
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
    @media (min-width: 1200px) and (min-resolution: 100dpi) {
      /* retina */

      .visual-holder .image-holder video {
        display: block;
        width: 105%;
        height: auto;
      }
    }
    @media (min-width: 1400px) {
      .vision-area .container {
        padding-left: 120px;
        max-width: 100%;
      }
      .vision-area.home-vision .container {
        padding: 0 0 0 120px;
      }
      .joinus-holder .text-wrap {
        margin-left: 80px;
      }
      .posts-holder.image-holder {
        margin-top: 0;
        width: 30%;
      }
      h1.typeit {
        min-height: 200px;
      }
      .home-holder.image-holder {
        margin-top: 40px;
        width: 45%;
      }
      .services-section.topsep .text-area p {
        font-size: ${t.fontSizes[2]};
      }
      .services-section.topsep .text-area h1 {
        margin-top: 0px;
      }
      .services-section.blog-content.topsep > .text-area {
        margin-top: 10px;
      }
      .partners-area2 {
        padding: 100px 0 0;
      }
      .col-area .description {
        max-width: 500px;
        margin: auto;
      }
      .image-holder img {
        width: 100%;
        height: 100%;
      }
      .about-section.learnmore .text-area {
        padding: 0px 120px;
        width: 60%;
      }
      .about-section.learnmore .btn {
        font-size: 100%;
      }
      .visual-holder {
        padding: 0 6%;
      }
      .vision-holder .btn {
        margin-bottom: 30px;
      }
      .biotech .text-area h1 {
        margin: 30px 0;
        width: 90%;
      }
      h2 {
        font-size: ${t.fontSizes[6]};
      }
      h3 {
        font-size: ${t.fontSizes[5]};
      }
      .contactus-holder {
        padding: 0px 50px;
      }
      .contactus-holder .text-area {
        width: 50%;
      }
      .contactus-holder .image-col {
        width: 35%;
      }
      .contactus-holder .text-list {
        margin: 0 0 60px;
      }
      .contactus-holder .text-list li {
        margin: 0 0 40px;
      }
      .col-area .image-holder {
        margin: 0 auto 40px;
      }
      .col-area .description h3 {
        margin: 0 0 15px;
      }
      .col-area .description p {
        margin: 0 0 35px;
      }
      .joinus-holder .title {
        font-size: 38px;
        line-height: 46px;
        margin: 0 0 30px;
      }
      .vision-holder .text-col {
        width: 35%;
        max-width: 600px;
      }
      .services-section .text-area {
        width: 52%;
        margin: 0 0 30px;
      }
      .services-section.contact-addr .text-area {
        width: 70%;
      }
      .section-wrap.key_indus {
        padding: 70px 35px;
      }
      .services-section.opensource .text-area {
        width: 52%;
        margin: 80px 0 30px;
      }
      .services-section.opensource1 .text-area {
        width: 70%;
        margin: 0 0 30px;
      }
      .services-section.opensource1 .text-area {
        width: 60%;
        margin: 0 0 30px;
      }
      .services-section.opensource1.blog-content.part2 .text-area {
        width: 100%;
        margin: 0 0 30px;
      }
      .services-section.biotech .text-area {
        width: 100%;
        margin: 0 0 30px;
      }
      .services-section.biotech .text-area {
        width: 100%;
        margin: 0 0 30px;
      }
      .method-area .text-area {
        width: 49%;
      }
    }
    @media (min-width: 1601px) {
      .fp-section .container {
        max-width: 100%;
      }
      .line-arrow.down {
        margin-top: 0px;
      }
      .about-section .text-area {
        position: relative;
        padding: 0 0 0 120px;
      }
    }
    @media (min-width: 1500px) {
      .home-holder.image-holder {
        margin-top: 10px;
      }
    }
    @media (max-width: 1499px) {
      .copyright-area {
        margin: 0;
      }
      .services-section .posts-holder.image-holder {
        margin-bottom: 40px;
      }
      .part2 .text-area p,
      .part2 .text-area h1,
      .part2 .text-area h2,
      .part2 .text-area h3,
      .part2 .text-area h4,
      .part2 .text-area ul,
      .part2 .text-area ol,
      .part2 .text-area center,
      .part2 .text-area .katex-display,
      main nav {
        padding: 0 0 0 60px;
      }
      .part2 .text-area h2 {
        padding: 0 0 0 60px;
      }
      pre {
        padding-left: 50px;
      }
      .blog-content a.btn.noarrow {
        font-size: ${t.fontSizes[0]};
        padding: 0px 20px;
        height: 30px;
      }
      .home-holder.image-holder,
      .posts-holder.image-holder {
        padding-right: 60px;
      }
      .vision-area .container {
        max-width: 100%;
        padding: 0;
      }
      .container {
        padding: 0 60px;
      }
      .services-section .text-area {
        position: relative;
        width: 49%;
        padding: 0 0 0 60px;
      }
      .services-section.blog-home > .text-area {
        padding: 0 0 0 60px;
        margin: 0;
      }
      .services-section.blog-content.topsep > .text-area {
        margin: 40px 0 0 60px;
        border-top: 1px solid var(--fg-color);
        padding-left: 0px;
      }
      .post_container {
        margin: 0 60px;
      }
      .about-section.use_case .text-area {
        width: 60%;
      }
      .services-section.use-cases.second .text-area2 {
        margin-right: 14%;
        max-width: 600px;
      }
      .vision-holder {
        padding: 60px 0 60px 50px;
      }
      .section-area {
        position: relative;
        padding: 130px 0 0px;
      }
      .menu > li {
        position: relative;
        margin: 0 15px;
      }
      .biotech .text-area2,
      .opensource .text-area2 {
        position: relative;
        width: 50%;
        padding: 30px 0;
      }
      .services-section.opensource1 .text-area {
        width: 70%;
      }
      .services-section.opensource1.blog-content .text-area {
        width: 55%;
      }
      .services-section.opensource1.blog-content.part2 .text-area {
        width: 100%;
      }
      .about-section.opensource .w70 {
        width: 60% !important;
      }
      .about-section.opensource .w30 {
        width: 30% !important;
        padding-right: 5%;
        padding-left: 5%;
      }
      .services-section .text-area {
        width: 70%;
        margin-bottom: 40px;
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
      .contactus-holder h2 {
        margin: 0 0 35px;
      }
      .contactus-holder .text-list {
        margin: 0 0 60px;
      }
      .contactus-holder .text-list li {
        margin: 0 0 40px;
      }
      .contactus-holder .text-list h3 {
        margin: 0 0 12px;
      }
      .btn {
        font-size: ${t.fontSizes[2]};
        line-height: 1.2;
        height: 44px;
        padding: 9px 18px;
      }
      .col-area .image-holder {
        width: 220px;
        margin: 0 auto 30px auto;
      }
      .joinus-holder {
        margin-right: 0;
      }
      .joinus-holder .title {
        font-size: ${t.fontSizes[4]};
        line-height: 40px;
        margin: 0 0 25px;
      }
      .partners-list .logo-wrap img {
        -webkit-transform: scale(0.85);
        -ms-transform: scale(0.85);
        transform: scale(0.85);
      }
      .vision-holder {
        margin-right: 0;
      }
      .vision-holder .text-col {
        width: 45%;
      }
      .vision-holder h2 {
        max-width: 360px;
        line-height: 1.2;
      }
      .method-area .text-area {
        width: 70%;
      }
    }
    @media (max-width: 767px) {
      .blog-content .article-text ul li:before {
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
      .blog-content .article-text ol,
      .blog-content .article-text ul li {
        display: block;
        padding-left: 35px;
        margin-bottom: 10px;
      }
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
      img.uc-rotate {
        transform: none;
      }
      main nav {
        margin: 30px 15px !important;
        padding: 0;
      }
      p.use-case-excerpt {
        margin: 0;
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
      .blog-content .article-text pre code {
        margin: 0;
      }
      .about-section .image-holder img {
        height: 100%;
        width: 100%;
      }
      .about-section .image-holder img {
        width: 190px;
        margin-bottom: 20px;
      }

      .about-section .image-holder {
        width: 100%;
        -webkit-box-ordinal-group: 2;
        -ms-flex-order: 1;
        order: 1;
        display: -webkit-box;
        display: -ms-flexbox;
        display: flex;
        -ms-flex-wrap: wrap;
        flex-wrap: wrap;
        -webkit-box-pack: end;
        -ms-flex-pack: end;
        justify-content: flex-end;
        margin: 0 0 30px;
      }
      .about-section .image-holder.image-holder4 {
        justify-content: flex-start;
      }
      .about-section .section-title {
        top: 0;
      }
      .about-section .text-area p.check .btn {
        padding: 8px;
        line-height: 0px;
        height: 20px;
        margin: 0 10px;
      }
      .about-section .text-area {
        padding: 0 15px;
        margin-bottom: 0;
      }

      .about-section .text-list li i {
        margin: 3px 5px 0 0;
      }
      .about-section .text-list li p {
        font-size: ${t.fontSizes[2]};
      }
      .about-section .text-list li {
        font-size: ${t.fontSizes[2]};
        margin: 0 0 10px;
      }
      .about-section .text-list {
        font-size: ${t.fontSizes[1]};
        line-height: 22px;
        position: relative;
        list-style: none;
        padding: 0;
        margin: 0px !important;
      }
      .about-section.biotech .contact.btn {
        margin: 30px 0;
      }
      .about-section.biotech .text-list li p {
        font-size: ${t.fontSizes[2]};
      }
      .about-section.biotech {
        padding: 60px 0 20px 0;
      }
      .about-section.learnmore .btn {
        font-size: 100%;
        height: auto;
      }
      .about-section.learnmore .split-col1,
      .about-section.learnmore .split-col2 {
        width: 48%;
      }
      .about-section.learnmore .text-area {
        padding: 0 15px;
        width: 100%;
      }
      .about-section.learnmore h3 {
        padding-top: 30px;
      }
      .about-section.opensource .w30 {
        width: 100% !important;
        padding-bottom: 20px;
        padding-left: 15px;
      }
      .about-section.opensource .w70 {
        width: 100% !important;
      }
      .about-section.opensource h1 {
        margin-top: 20px;
      }
      .about-section.opensource {
        font-size: ${t.fontSizes[2]};
      }
      .about-section.opensource4 {
        padding: 20px 0;
      }
      .about-section.use_case .text-area h1 {
        text-align: left;
      }
      .about-section.use_case .text-area p {
        font-size: ${t.fontSizes[2]};
        margin-top: 20px;
      }
      .about-section.use_case .text-area {
        width: 100%;
        padding: 0 15px;
      }
      .backdrop {
        animation: 8s scroll_r infinite linear;
        background-size: contain;
        min-height: 200px;
      }
      .biotech .text-area h1 {
        padding: 0;
        margin: 0;
      }
      .btn {
        font-size: ${t.fontSizes[1]};
        margin: 0;
      }
      .btn {
        font-size: ${t.fontSizes[2]};
        padding: 8px 18px;
      }
      .col-area .description .btn {
        margin-left: 15px;
      }
      .col-area .description h3 {
        text-align: left;
        padding: 0 15px;
      }
      .col-area .description p {
        text-align: left;
        padding: 0 15px;
      }
      .col-area .description {
        font-size: ${t.fontSizes[2]};
        text-align: left;
        padding: 0;
      }
      .col-area .image-holder img {
        margin-left: 15px;
      }
      .col-area .image-holder {
        width: 220px;
        margin: 0 0 30px 0;
      }
      .col-area .info-col {
        width: 100%;
        margin: 0 0 60px;
      }
      .col-area .info-col2 {
        padding: 30px 15px 0;
      }
      .col-area .info-col2,
      .col-area .info-col1 {
        width: 100%;
      }
      .col-area .info-col:last-child {
        margin-bottom: 40px;
      }
      .col-area h2 {
        margin: 40px 0 40px;
        text-align: left;
        padding: 0 15px;
      }
      .contact-addr .col-area .description h3 {
        text-align: left;
        padding: 0;
      }
      .contact-addr .col-area .info-col {
        margin: 0;
      }
      .contact-addr .hello {
        padding: 0;
      }
      .contact-addr.section-wrap .text-wrap {
        margin: 0;
      }
      .contact-addr.services-section .text-area h3 {
        margin: 30px 0 0 0;
        font-size: ${t.fontSizes[6]};
      }
      .contact-field input,
      .contact-field textarea {
        font-size: ${t.fontSizes[3]};
      }
      .contact-field.services-section {
        padding: 60px 0 0 0;
      }
      .contactus-holder .image-col img {
        margin-left: -30px;
      }
      .contactus-holder .image-col {
        display: -webkit-box;
        display: -ms-flexbox;
        display: flex;
        -ms-flex-wrap: wrap;
        flex-wrap: wrap;
        width: 100%;
        margin: 30px 0 50px 0;
        padding: 0;
      }
      .contactus-holder .image-holder {
        -webkit-box-ordinal-group: 2;
        -ms-flex-order: 1;
        order: 1;
        width: 100%;
        margin: 0 auto 40px 30px;
        padding: 0 15px;
      }
      .contactus-holder .text-area {
        text-align: left;
        width: 100%;
        margin: 0 0 40px;
        padding: 0 15px;
      }
      .contactus-holder .text-list {
        font-size: ${t.fontSizes[0]};
      }

      .contactus-holder h2 {
        text-align: left;
        width: 100%;
        -webkit-box-ordinal-group: 3;
        -ms-flex-order: 2;
        order: 2;
        max-width: inherit;
        margin: 0;
        padding: 0 15px;
      }
      .contactus-holder {
        display: block;
      }
      .container {
        padding: 0;
      }
      .copyright {
        margin: 0 0 20px;
      }
      .copyright-area {
        display: block;
        padding: 25px;
        margin: 0px -15px 25px;
      }
      .copyright-col1,
      .copyright-col2 {
        width: 100%;
      }
      .f-logo {
        width: 120px;
      }
      .hideresp {
        display: none !important;
      }
      .home-holder.highlight.image-holder img {
        width: 100%;
        margin: 0;
        padding: 0;
      }
      .home-holder.highlight.image-holder {
        display: block;
        margin: 0 0 20px;
        padding: 0;
      }
      .inner .fixed-header .logo,
      .inner .fixed-header .menu {
        transform: scale(1) !important;
        margin-top: 0;
      }
      .joinus-area .container {
        padding: 0;
      }
      .joinus-area.section-wrap {
        padding: 10px 0;
      }
      .joinus-holder .image-holder .image-wrap {
        width: 280px;
        margin: 0 -15px 0 0;
      }

      .joinus-holder .image-holder {
        -webkit-box-ordinal-group: 2;
        -ms-flex-order: 1;
        order: 1;
        margin: 0 0 30px;
        display: flex;
        width: 100%;
        justify-content: flex-end;
      }

      .joinus-holder .text-wrap {
        text-align: left;
        -webkit-box-ordinal-group: 3;
        -ms-flex-order: 2;
        order: 2;
        width: 100%;
        padding: 0 15px;
        margin: 20px 0 10px 0;
      }

      .joinus-holder .title {
        font-size: ${t.fontSizes[3]};
        line-height: 30px;
        margin: 0 0 50px 0;
      }
      .joinus-holder {
        margin: 30px 0;
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
      .method-area .image-holder img {
        width: 200px;
      }
      .method-area .image-holder {
        width: 100%;
        margin: 0 0 30px;
      }
      .method-area .text-area h2::after {
        content: "";
        display: block;
        width: 100px;
        height: 1px;
        border-bottom: 1px solid var(--fg-color);
        padding-top: 40px;
        margin: 0;
      }
      .method-area .text-area {
        padding: 0 15px;
        width: 100%;
        text-align: left;
        margin: 0 0 30px;
      }
      .part2 .text-area h2 {
        padding: 0 !important;
        margin: 40px 15px 20px !important;
        width: 90% !important;
      }
      .part2 .text-area p,
      .part2 .text-area h1,
      .part2 .text-area h2,
      .part2 .text-area h3,
      .part2 .text-area h4,
      .part2 .text-area ol,
      .part2 .text-area ul,
      .part2 .text-area center,
      .part2 .text-area .katex-display,
      main nav {
        margin: 20px 15px;
        padding: 0 15px;
        width: 100%;
      }
      .part2 .text-area ol {
        margin: 20px 30px;
      }
      .part2 .text-area ul {
        padding: 0 15px;
        margin: 30px 0;
      }
      .partners-area h2 {
        margin: 30px 0 30px 0;
        padding: 0 15px;
        text-align: left;
      }
      .partners-list .logo-wrap img {
        -webkit-transform: scale(0.7);
        -ms-transform: scale(0.7);
        transform: scale(0.7);
      }
      .partners-list li {
        margin: 0 0 20px;
        width: 33.333%;
      }
      .post_container {
        margin: 0 15px;
      }
      .post_content {
        width: 100%;
        display: block;
        height: auto;
      }
      .section {
        padding: 0;
        margin-top: 0;
      }
      .section-area {
        padding: 65px 0 0px;
        margin-bottom: 30px;
      }
      .section-area:before {
        left: 15px;
        right: 15px;
      }
      .section-title {
        position: relative;
        top: 0;
        -webkit-transform: none;
        -ms-transform: none;
        transform: none;
        margin: 0 0 25px;
      }
      .section-title.homepost {
        padding: 0;
        margin: 0;
      }
      .section-wrap {
        padding: 0;
        margin-bottom: 0;
      }
      .section-wrap.biotech {
        padding: 0;
        margin-bottom: 30px;
      }
      .section-wrap.key_indus {
        position: relative;
        padding: 0;
        margin-bottom: 30px;
        overflow: hidden;
      }
      .services-section .image-holder img {
        width: 150px;
      }
      .services-section .image-holder {
        width: 100%;
        -webkit-box-ordinal-group: 2;
        -ms-flex-order: 1;
        order: 1;
        display: -webkit-box;
        display: -ms-flexbox;
        display: flex;
        -ms-flex-wrap: wrap;
        flex-wrap: wrap;
        -webkit-box-pack: end;
        -ms-flex-pack: end;
        justify-content: flex-end;
        margin: 0 0 30px;
      }
      .services-section .image-holder3 {
        display: none;
        margin: 0 0 30px;
      }
      .services-section .text-area h1 {
        margin: 20px 0 35px;
      }
      .services-section .text-area {
        padding: 0 15px;
        width: 100%;
        -webkit-box-ordinal-group: 3;
        -ms-flex-order: 2;
        text-align: left;
      }
      .services-section .text-wrap p {
        font-size: ${t.fontSizes[2]};
      }
      .services-section .text-wrap {
        font-size: ${t.fontSizes[1]};
        line-height: 1.2;
        padding-bottom: 20px;
        max-width: inherit;
      }
      .services-section.blog-content .text-wrap {
        margin-bottom: 0;
        padding-bottom: 0;
      }
      .services-section.blog-content.part2 .text-area {
        padding: 0;
        width: 100%;
      }
      .services-section.blog-content.topsep > .text-area {
        width: 100%;
        padding: 0;
        margin: 0 15px;
        border: 0px;
      }
      .services-section.blog-home > .text-area {
        margin: 0;
        padding: 0 15px;
        text-align: left;
      }
      .services-section.biotech .image-holder.image-holder2 img {
        width: 250px;
        margin-bottom: 50px;
      }
      .services-section.biotech .image-holder2 {
        display: block;
        margin: 0;
        justify-content: flex-start;
      }
      .services-section.biotech .text-area .text-wrap {
        margin: 30px 0 0;
      }
      .services-section.biotech .text-area h3 {
        margin: 0 0 20px;
      }
      .services-section.biotech .text-area {
        margin: 0;
      }
      .services-section.biotech .text-area2 {
        margin: 0 0 30px;
        padding: 0 15px;
        width: 100%;
        text-align: left;
      }
      .services-section.opensource1 .text-area {
        margin: 0;
        width: 100%;
      }
      .services-section.opensource1.blog-content .text-area,
      .services-section.opensource1.blog-content.part2 .text-area {
        width: 100%;
      }
      .services-section.use_case .text-area {
        margin: 0;
      }
      .services-section.use_case .text-wrap {
        padding-bottom: 0;
      }
      .services-section.use_case.first {
        flex-direction: column-reverse;
      }
      .services-section.use-cases .image-holder.image-holder2 {
        display: none;
      }
      .services-section.use-cases.second .text-area2 {
        padding: 0 15px;
        margin: 0;
      }
      .services-section.use-cases.second .text-wrap {
        margin-bottom: 30px;
      }
      .services-section.use-cases.second {
        margin: 0;
      }
      .use_case .image-holder img {
        display: block;
        margin-right: 0;
        width: 50%;
        margin: 30px 0;
      }
      .use_case .image-holder {
        display: block;
        margin-right: 0;
        width: 100%;
        margin-top: 0px;
        display: flex;
      }
      .use_case.bloc1,
      .use_case.bloc2 {
        padding: 40px 0;
      }
      .use_case.results .image-holder {
        margin: 0;
      }
      .use_case.results ul {
        margin-top: 0;
      }
      .use_case.results {
        padding: 0;
      }
      .vision-area .container {
        padding: 0;
      }
      .vision-area {
        padding: 0;
      }
      .vision-holder .image-holder .image-wrap {
        width: 280px;
        margin: 0 -15px 0 0;
      }
      .vision-holder .image-holder {
        width: 100%;
        -webkit-box-ordinal-group: 2;
        -ms-flex-order: 1;
        order: 1;
        margin: 30px auto 50px;
        display: flex;
        justify-content: flex-end;
      }
      .vision-holder .text-col {
        -webkit-box-ordinal-group: 3;
        -ms-flex-order: 2;
        order: 2;
        width: 100%;
        display: block;
        text-align: left;
        padding: 0 15px;
        margin: 0 auto 40px;
      }
      .vision-holder h2 {
        max-width: inherit;
        width: 100%;
        margin: 0 0 10px;
        text-align: left;
      }
      .vision-holder {
        margin-right: 0;
        padding: 10px 0;
      }
      .visual-holder .image-holder {
        width: 280px;
        margin: 30px auto 50px;
      }
      .visual-holder .visual-caption {
        font-size: ${t.fontSizes[1]};
        line-height: 24px;
        width: 100%;
        text-align: center;
        margin-bottom: 30px;
        padding: 0 15px;
      }
      .visual-holder h1 {
        margin: 0 0 10px;
        height: 80px;
      }
      .visual-holder {
        display: block;
        padding: 0;
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

/** @jsx jsx */
import { jsx, useThemeUI } from "theme-ui"

function BlogPostContent({ dangerouslySetInnerHTML }) {
  const { theme: t } = useThemeUI()
  return (
    <div
      className="blog-post-content"
      dangerouslySetInnerHTML={dangerouslySetInnerHTML}
      css={`
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

        iframe {
          max-height: 90%;
          width: 100%;

          @media (min-width: ${t.breakpoints[1]}) {
            margin-left: 60px;
            width: 60%;
          }

          @media (min-width: ${t.breakpoints[5]}) {
            margin-left: 120px;
          }
        }

        table {
          margin: 0px 15px;
          display: block;
          overflow-x: auto;
          white-space: nowrap;
          margin-bottom: 20px;
          @media (min-width: ${t.breakpoints[1]}) {
            margin: 0px;
            margin-bottom: 20px;
            margin-left: 60px;
          }

          @media (min-width: ${t.breakpoints[5]}) {
            margin-left: 120px;
            margin-bottom: 20px;
          }
          td {
            max-width: 100%;
            code {
              white-space: nowrap;
            }
          }
          th {
            max-width: 100%;
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
          overflow: auto;

          ::-webkit-scrollbar {
            -webkit-appearance: none;
          }

          ::-webkit-scrollbar:horizontal {
            height: 8px;
          }

          ::-webkit-scrollbar-thumb {
            background-color: rgba(0, 0, 0, 0.3);
            border-radius: 10px;
            border: 2px solid #ffffff;
          }

          ::-webkit-scrollbar-track {
            border-radius: 10px;
            background-color: #ffffff;
          }

          background-color: #ecece9;
          font-size: ${t.fontSizes[2]};
          border-left: 15px solid ${t.colors.orange};
          margin-bottom: 30px;
          margin-top: 30px;

          @media (min-width: ${t.breakpoints[1]}) {
            padding: 0 0 0 50px;
          }

          code {
            display: block;
            line-height: 1.75;
            font-size: 16px;
            padding: 20px 15px;
            min-width: max-content;
            width: 100%;
            max-width: 1000px;

            @media (min-width: ${t.breakpoints[1]}) {
              font-size: 18px;
              min-width: fit-content;
              width: 100%;
              max-width: none;
            }

            @media (min-width: ${t.breakpoints[5]}) {
              padding: 20px 55px;
            }
          }
        }

        blockquote {
          border-left: 15px solid ${t.colors.orange};
          padding-left: 20px;

          @media (min-width: ${t.breakpoints[1]}) {
            margin-left: 70px;
            width: 65%;
            max-width: 1000px;
          }

          @media (min-width: ${t.breakpoints[5]}) {
            margin-left: 130px;
          }

          p {
            padding: 0;
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

          @media (min-width: ${t.breakpoints[1]}) {
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
          padding: 0px;
          margin: 20px 15px;
          width: 100%;
          max-width: 1000px;

          @media (min-width: ${t.breakpoints[1]}) {
            margin: 0px 0px 30px;
            padding-left: 60px;
            width: 65%;
          }

          @media (min-width: ${t.breakpoints[5]}) {
            padding-left: 120px;
          }
        }

        h4,
        h5 {
          padding: 0px;
          margin: 20px 15px;
          width: 65%;
          max-width: 1000px;

          @media (min-width: ${t.breakpoints[1]}) {
            margin: 0px 0px 15px;
            padding-left: 60px;
          }
          @media (min-width: ${t.breakpoints[5]}) {
            padding-left: 120px;
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
            counter-increment: none;
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

        img {
          display: block;
          margin-left: auto;
          margin-right: auto;
          width: 50%;
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

        // These are the styles used for code highlighting. The classes below
        // are defined by PrismJS, with the highlighting below being based on
        // prism-vs.
        //
        // Some colors were modified to pass most tests from here:
        //
        //     https://wave.webaim.org/
        //
        // using this tools as a helper:
        //
        //     https://contrastchecker.com
        //

        .token.comment,
        .token.prolog,
        .token.doctype,
        .token.cdata {
          color: #763333;
          font-style: italic;
        }

        .token.namespace {
          opacity: 0.7;
        }

        .token.string {
          color: #a31515;
        }

        .token.punctuation,
        .token.operator {
          color: #393a34; /* no highlight */
        }

        .token.url,
        .token.symbol,
        .token.number,
        .token.boolean,
        .token.variable,
        .token.constant,
        .token.inserted {
          color: #037708;
        }

        .token.atrule,
        .token.keyword,
        .token.attr-value,
        .language-autohotkey .token.selector,
        .language-json .token.boolean,
        .language-json .token.number,
        code[class*="language-css"] {
          color: #0000ff;
        }

        .token.function {
          color: #393a34;
        }

        .token.deleted,
        .language-autohotkey .token.tag {
          color: #9a050f;
        }

        .token.selector,
        .language-autohotkey .token.keyword {
          color: #00009f;
        }

        .token.important {
          color: #e90;
        }

        .token.important,
        .token.bold {
          font-weight: bold;
        }

        .token.italic {
          font-style: italic;
        }

        .token.class-name,
        .language-json .token.property {
          color: #2b91af;
        }

        .token.tag,
        .token.selector {
          color: #800000;
        }

        .token.attr-name,
        .token.property,
        .token.regex,
        .token.entity {
          color: #ff0000;
        }

        .token.directive.tag .tag {
          background: #ffff00;
          color: #393a34;
        }
      `}
    />
  )
}

export default BlogPostContent

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

        table {
          margin: 0px 15px;
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
          overflow-x: visible;

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
        pre {
          background-color: #ecece9;
          font-size: ${t.fontSizes[2]};
          border-left: 15px solid ${t.colors.orange};
          margin-bottom: 30px;
          margin-top: 30px;
          padding-left: 50px;
          // overflow: hidden;
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
      `}
    />
  )
}

export default BlogPostContent

/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link } from "gatsby"
import { Box, Text } from "theme-ui"

import { CallToActionFooter, Layout, SEO } from "../../components"

import { Manifesto, Communities } from "./components"

const OpenSourcePage = () => {
  return (
    <Layout
      fullPageFooter
      footer={
        <Box className="section viewport-section s_grey transition-section">
          <CallToActionFooter
            title={`Ready to achieve your big vision?`}
            backdropVariant={5}
            transitionClass={`transition-section__transition--slide-fade-in`}
            customWrapperSx={{
              py: [`40px`, `40px`, `60px`],
            }}
          />
        </Box>
      }
    >
      <SEO title="Our open source projects" />
      <Manifesto />
      <Communities />
      <Box
        className="section s_white viewport-section transition-section"
        sx={{
          py: [`20px`, `20px`, `90px`],
          px: [`15px`, `15px`, `60px`, `120px`],
        }}
      >
        <Text
          className="check transition-section__transition--slide-fade-in bottom-in"
          sx={{
            fontSize: [`16px`, `16px`, `27px`],
            lineHeight: [1.15, 1.15, 1.2],
          }}
        >
          Check out our
          <a
            sx={{
              lineHeight: [
                `0px !important`,
                `0px !important`,
                `1.2 !important`,
              ],
              padding: [
                `8px !important`,
                `8px !important`,
                `9px 18px !important`,
              ],
              height: [`20px`, `20px`, `44px`],
              fontSize: [
                `18px !important`,
                `18px !important`,
                null,
                null,
                null,
                `24px !important`,
              ],
              m: [
                `0 10px !important`,
                `0 10px !important`,
                `-10px 15px !important`,
              ],
              verticalAlign: `top`,
            }}
            className="button button-secondary button-medium  min-5__button-large"
            href="https://github.com/tweag"
          >
            Github page
          </a>
          or read our
          <Link
            to="/blog"
            sx={{
              lineHeight: [
                `0px !important`,
                `0px !important`,
                `1.2 !important`,
              ],
              padding: [
                `8px !important`,
                `8px !important`,
                `9px 18px !important`,
              ],
              height: [`20px`, `20px`, `44px`],
              fontSize: [
                `18px !important`,
                `18px !important`,
                null,
                null,
                null,
                `24px !important`,
              ],
              m: [
                `0 10px !important`,
                `0 10px !important`,
                `-10px 15px !important`,
              ],
              verticalAlign: `top`,
            }}
            className="button button-secondary button-medium  min-5__button-large"
          >
            Blog
          </Link>
          to learn more
        </Text>
      </Box>
    </Layout>
  )
}

export default OpenSourcePage

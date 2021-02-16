/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link } from "gatsby"
import { Box, Text } from "theme-ui"

import { CallToActionFooter, Layout, SEO } from "../../components"

import { Manifesto, Communities } from "./components"

const OpenSourcePage = () => {
  return (
    <Layout>
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
              lineHeight: [`0px`, `0px`, 1.2],
              padding: [`8px`, `8px`, `9px 18px`],
              height: [`20px`, `20px`, `44px`],
              m: [
                `0 10px !important`,
                `0 10px !important`,
                `-10px 15px !important`,
              ],
            }}
            className="btn noarrow"
            href="https://github.com/tweag"
          >
            Github page
          </a>
          or read our
          <Link
            to="/blog"
            className="btn noarrow"
            sx={{
              lineHeight: [`0px`, `0px`, 1.2],
              padding: [`8px`, `8px`, `9px 18px`],
              height: [`20px`, `20px`, `44px`],
              m: [
                `0 10px !important`,
                `0 10px !important`,
                `-10px 15px !important`,
              ],
            }}
          >
            Blog
          </Link>
          to learn more
        </Text>
      </Box>
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
    </Layout>
  )
}

export default OpenSourcePage

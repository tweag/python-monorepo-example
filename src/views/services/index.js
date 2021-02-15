/** @jsx jsx */
import { jsx, Box } from "theme-ui"
import { CallToActionFooter, Divider, Layout, SEO } from "../../components"
import { Services, Quote, WhatWeDo, KeyIndustries } from "./components"

const ServicesPage = () => {
  return (
    <Layout>
      <SEO title="Our services" />
      <Services />
      <Quote />
      <WhatWeDo />
      <Divider
        customSx={{
          mx: [`20px`, `20p`, `60px`],
          my: [`60px`],
        }}
      />
      <KeyIndustries />
      <Box
        className="section viewport-section s_orange transition-section"
        sx={{
          mt: [`30px`, `30px`, `60px`],
        }}
      >
        <CallToActionFooter
          title={`Ready to achieve your big vision?`}
          backdropVariant={3}
          transitionClass={`transition-section__transition--slide-fade-in`}
          customWrapperSx={{
            py: [`40px`, `40px`, `60px`],
          }}
        />
      </Box>
    </Layout>
  )
}

export default ServicesPage

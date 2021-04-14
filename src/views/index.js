/** @jsx jsx */
import { jsx } from "theme-ui"

import { FullPageLayout } from "../layouts"
import { SEO, CallToActionFooter, Footer } from "../components"

import { Section1, Section2, Section3, Section4, Section5 } from "./components"

const IndexPage = () => {
  return (
    <FullPageLayout>
      <SEO title="Software innovation lab" />
      <div className="section s_white">
        <Section1 />
      </div>
      <div className="section s_yellow">
        <Section2 />
      </div>
      <div className="section s_white">
        <Section3 />
      </div>
      <div className="section s_purple">
        <Section4 />
      </div>
      <div className="section s_white">
        <Section5 />
      </div>
      <div className="section s_red">
        <div className="s_red">
          <CallToActionFooter
            title={`Ready to achieve your big vision?`}
            backdropVariant={2}
            transitionClass={`transition--slide-fade-in`}
          />
        </div>
        <Footer />
      </div>
    </FullPageLayout>
  )
}

export default IndexPage

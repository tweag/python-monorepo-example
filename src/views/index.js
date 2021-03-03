/** @jsx jsx */
import { jsx } from "theme-ui"

import LayoutFullPage from "../components/layout-fullpage"
import Footer from "../components/footer"
import { SEO, CallToActionFooter } from "../components"

import { Section1, Section2, Section3, Section4, Section5 } from "./components"

const IndexPage = () => {
  return (
    <LayoutFullPage>
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
            customWrapperSx={{
              py: [`40px`, `40px`, `60px`],
            }}
          />
        </div>
        <Footer />
      </div>
    </LayoutFullPage>
  )
}

export default IndexPage

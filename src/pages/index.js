/** @jsx jsx */
import { jsx } from "theme-ui"

import Footer from "../components/footer"
import LayoutFullPage from "../components/layout-fullpage"
import SEO from "../components/seo"

import {
  Section1,
  Section2,
  Section3,
  Section4,
  Section5,
  Section6,
} from "./_components/_index"

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
          <Section6 />
        </div>
        <Footer />
      </div>
    </LayoutFullPage>
  )
}

export default IndexPage

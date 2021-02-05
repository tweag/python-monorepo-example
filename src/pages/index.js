/** @jsx jsx */
import { jsx } from "theme-ui"

import Footer from "../components/footer"
import KeyIndustries from "../components/keyindustries"
import LayoutFullPage from "../components/layout-fullpage"
import SEO from "../components/seo"

import homePreview from "../images/home_preview.png"
import homeVideo from "../images/home.mp4"
import visionaries from "../images/img2.gif"
import logoAmgen from "../images/logo_amgen.png"
import logoCea from "../images/logo_cea.png"
import logoGoogle from "../images/logo_google.png"
import logoPfizer from "../images/logo_pfizer.png"
import logoTarget from "../images/logo_target.png"
import logoOrange from "../images/logo_orange.png"

import {
  Section1,
  Section2,
  Section4,
  Section5,
  Section6,
} from "./_components/_index"

const IndexPage = () => {
  return (
    <LayoutFullPage>
      <SEO title="Software innovation lab" />
      <div className="section s_white">
        <Section1 homePreview={homePreview} homeVideo={homeVideo} />
      </div>
      <div className="section s_yellow">
        <Section2 visionaries={visionaries} />
      </div>
      <div className="section s_white">
        <KeyIndustries />
      </div>
      <div className="section s_purple">
        <Section4 />
      </div>
      <div className="section s_white">
        <Section5
          partners={[
            {
              src: logoAmgen,
              name: `Amgen`,
            },
            {
              src: logoCea,
              name: `CEA`,
            },
            {
              src: logoGoogle,
              name: `Google`,
            },
            {
              src: logoOrange,
              name: `Orange`,
            },
            {
              src: logoPfizer,
              name: `Pfizer`,
            },
            {
              src: logoTarget,
              name: `Target`,
            },
          ]}
        />
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

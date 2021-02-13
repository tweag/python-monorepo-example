/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"

import { ListIndustries } from "../../../components"

import biotech from "../../../images/img3.gif"
import fintech from "../../../images/img4.gif"
import vehicles from "../../../images/img5.gif"

const industries = [
  {
    h: <Fragment>Biotech</Fragment>,
    p: (
      <Fragment>
        Build statistical models, iterate on them quickly and increase
        productivity.
      </Fragment>
    ),
    src: biotech,
    link: `/industry/biotech`,
  },
  {
    h: <Fragment>Fintech</Fragment>,
    p: (
      <Fragment>
        Minimize risk with high-assurance software, from blockchain to trading
        systems.
      </Fragment>
    ),
    src: fintech,
    link: `/industry/fintech`,
  },
  {
    h: <Fragment>Autonomous vehicles</Fragment>,
    p: (
      <Fragment>
        Put safety first with robust and reliable software, backed by static
        analysis.
      </Fragment>
    ),
    src: vehicles,
    link: `/industry/autonomous`,
  },
]

function KeyIndustries() {
  return (
    <div
      className="section s_white viewport-section transition-section"
      sx={{
        maxWidth: `1440px`,
        margin: `auto`,
      }}
    >
      <ListIndustries
        industries={industries}
        transitionClass={`transition-section__transition--slide-fade-in`}
      />
    </div>
  )
}

export default KeyIndustries

import React from "react"
import ReactFullpage from "@fullpage/react-fullpage"
import { Global } from "@emotion/react"

import JQuery from "./jquery"
import { Navigation } from "../components"
import { globalStyles } from "../styles/global"

import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

const FULLPAGE_LICENSE_KEY = `E12D894A-7B8349EE-B967F95D-3FF82929`

const LayoutFullPage = ({ children }) => {
  // Here we synchronise the header class with the visible section, so that
  // it can be styled to contrast appropriately with the background
  const [isResponsive, setResponsive] = React.useState(false)
  const [inverseHeader, setInverseHeader] = React.useState(false)
  const onLeaveSection = function (origin, destination, direction) {
    // In responsive mode, the fullpage component doesn't track the
    // current page well, and we allow the jquery sticky header plugin
    // to ensure that the header behaviour is the same as on other pages
    if (!isResponsive) {
      for (const name of destination.item.className.split(/\s+/)) {
        if (name.startsWith(`s_`)) {
          setInverseHeader([`s_purple`, `s_black`].includes(name))
        }
      }
    }
  }
  return (
    <div id="wrapper" className="home">
      <Global styles={globalStyles} />
      <JQuery />
      <Navigation inverted={inverseHeader} fullpage={true} />
      <ReactFullpage
        licenseKey={FULLPAGE_LICENSE_KEY}
        responsiveWidth={769}
        onLeave={onLeaveSection}
        afterResponsive={setResponsive}
        render={({ state, fullpageApi }) => {
          return <main>{children}</main>
        }}
      />
    </div>
  )
}

export default LayoutFullPage

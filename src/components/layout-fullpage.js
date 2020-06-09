import React from "react"
import ReactFullpage from "@fullpage/react-fullpage"
import { Global } from "@emotion/core"

import Header from "./header"
import Footer from "./footer"
import { globalStyles } from "../styles/global"

import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

const FULLPAGE_LICENSE_KEY = `E12D894A-7B8349EE-B967F95D-3FF82929`

const LayoutFullPage = ({ children }) => {
  return (
    <div id="wrapper" className="home">
      <Header />
      <Global styles={globalStyles} />
      <ReactFullpage
        licenseKey={FULLPAGE_LICENSE_KEY}
        responsiveWidth={769}
        render={({ state, fullpageApi }) => {
          return (
            <>
              <main>{children}</main>
              <Footer />
            </>
          )
        }}
      />
    </div>
  )
}

export default LayoutFullPage

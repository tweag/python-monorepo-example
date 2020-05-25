import React from "react"
import ReactFullpage from "@fullpage/react-fullpage"

import Header from "./header"
import Footer from "./footer"

import "normalize.css"
import "./layout.scss"

const FULLPAGE_LICENSE_KEY = "E12D894A-7B8349EE-B967F95D-3FF82929"

const LayoutFullPage = ({ children }) => {
  return (
    <div id="wrapper" className="home">
      <Header />
      <ReactFullpage
        licenseKey={FULLPAGE_LICENSE_KEY}
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

import React from "react"
import { Global } from "@emotion/core"

import JQuery from "./jquery"
import Navigation from "./navigation"
import Footer from "./footer"
import { globalStyles } from "../styles/global"

import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

const Layout = ({ children }) => {
  return (
    <div id="wrapper" className="inner">
      <Global styles={globalStyles} />
      <JQuery />
      <Navigation />
      <main>{children}</main>
      <Footer />
    </div>
  )
}

export default Layout

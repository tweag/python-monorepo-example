import React from "react"
import { Global } from "@emotion/core"

import Header from "./header"
import Footer from "./footer"
import { globalStyles } from "../styles/global"

import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

const Layout = ({ children }) => {
  return (
    <div id="wrapper" className="inner">
      <Header />
      <Global styles={globalStyles} />
      <main>{children}</main>
      <Footer />
    </div>
  )
}

export default Layout

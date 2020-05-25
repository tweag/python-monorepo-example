import React from "react"

import Header from "./header"
import Footer from "./footer"

import "normalize.css"
import "./layout.scss"

const Layout = ({ children }) => {
  return (
    <div id="wrapper" className="inner">
      <Header />
      <main>{children}</main>
      <Footer />
    </div>
  )
}

export default Layout

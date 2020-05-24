import React, { useEffect } from "react"
import { withPrefix } from "gatsby"

import Header from "./header"
import Footer from "./footer"

import "normalize.css"
import "./layout.scss"

const addScript = attrs => {
  const script = document.createElement("script")
  for (const attr in attrs) {
    script[attr] = attrs[attr]
  }
  script.async = false
  script.defer = false
  document.body.append(script)
}

const Layout = ({ children, wrapperClass }) => {
  useEffect(() => {
    // Don't add these in <head>. Instead, dynamically insert
    // these scripts, so that they can run at exactly the right
    // time, i.e. once the component has been mounted.
    addScript({
      src: "https://code.jquery.com/jquery-3.3.1.min.js",
      integrity: "sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8=",
      crossOrigin: "anonymous",
    })
    addScript({ src: withPrefix("/jquery.main.js") })
  }, [])
  return (
    <div id="wrapper" className={wrapperClass}>
      <Header />
      <main>{children}</main>
      <Footer />
    </div>
  )
}

Layout.defaultProps = {
  wrapperClass: `inner`,
}

export default Layout

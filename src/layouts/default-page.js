/** @jsx jsx */
import { jsx, Box } from "theme-ui"
import { Global } from "@emotion/react"

import JQuery from "./jquery"
import { Navigation, Footer } from "../components"
import { globalStyles } from "../styles/global"

import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

import { useEffect } from "react"

const Layout = ({ children, footer = <div></div>, fullPageFooter = false }) => {
  preventScrollJumpOnRefresh()

  return (
    <div id="wrapper" className="inner">
      <Global styles={globalStyles} />
      <JQuery />
      <Navigation />
      <main>{children}</main>
      <Box
        sx={{
          verticalAlign: `top`,
          background: `black`,
          minHeight: [fullPageFooter ? `calc(100vh)` : null],
        }}
      >
        {footer}
        <Footer />
      </Box>
    </div>
  )
}

// Necessary to prevent Chromium-based browsers from jumping
// past animated transitions on page reload
const preventScrollJumpOnRefresh = () => {
  useEffect(() => {
    window.history.scrollRestoration = `manual`
  }, [])
}

export default Layout

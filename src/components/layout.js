/** @jsx jsx */
import { jsx, Box } from "theme-ui"
import { Global } from "@emotion/core"

import JQuery from "./jquery"
import Header from "./navigation"
import Footer from "./footer"
import { globalStyles } from "../styles/global"

import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

const Layout = ({ children, footer, fullPageFooter = false }) => {
  return (
    <div id="wrapper" className="inner">
      <Global styles={globalStyles} />
      <JQuery />
      <Header />
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

export default Layout

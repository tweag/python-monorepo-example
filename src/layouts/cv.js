import React from "react"
import { Global } from "@emotion/core"

import JQuery from "../components/jquery"
import { globalStyles } from "../styles/global"

import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

const CvLayout = ({ children }) => {
  return (
    <div>
      <Global styles={globalStyles} />
      <JQuery />
      <main>{children}</main>
    </div>
  )
}

export default CvLayout

import React from "react"
import { Global } from "@emotion/core"

import JQuery from "./jquery"
import { globalStyles } from "../styles/global"

import { AntiIndex } from "../components/anti-index"
import "normalize.css"
import "../fonts/Stratos.css"
import "../fonts/icomoon.css"

const CvLayout = ({ children }) => {
  return (
    <div>
      <AntiIndex />
      <Global styles={globalStyles} />
      <JQuery />
      <main>{children}</main>
    </div>
  )
}

export default CvLayout

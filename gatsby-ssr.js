import React from "react"
export function onRenderBody({ setHeadComponents }) {
  setHeadComponents([
    <script
      key="abc"
      type="text/javascript"
      src="https://abc.com/abc/abc0123.js"
    />,
  ])
}

/* eslint-disable react/jsx-key */
import { React } from "react"

const headComponents = [
  <script
    src="https://cdn.cookielaw.org/scripttemplates/otSDKStub.js"
    type="text/javascript"
    charset="UTF-8"
    data-domain-script="3a05f346-a11c-458d-9bec-8c7fb98aa3aa"
  ></script>,
  <script type="text/javascript">function OptanonWrapper() {}</script>,
]

export const onRenderBody = ({ setHeadComponents }) => {
  setHeadComponents(headComponents)
}

import React, { useEffect } from "react"
import { withPrefix } from "gatsby"

const addScript = attrs => {
  const script = document.createElement(`script`)
  for (const attr in attrs) {
    if({}.hasOwnProperty.call(attrs, attr)) {
      script[attr] = attrs[attr]
    }
  }
  script.async = false
  script.defer = false
  document.body.append(script)
}

/**
 * Inject jQuery code. It will run once the new DOM has been mounted.
 */
const JQuery = () => {
  useEffect(() => {
    // Don't add these in <head>. Instead, dynamically insert
    // these scripts, so that they can run at exactly the right
    // time, i.e. once the component has been mounted.
    addScript({
      src: `https://code.jquery.com/jquery-3.3.1.min.js`,
      integrity: `sha256-FgpCb/KJQlLNfOu91ta32o/NMZxltwRo8QtmkMRdAu8=`,
      crossOrigin: `anonymous`,
    })
    addScript({ src: withPrefix(`/jquery.main.js`) })
  }, [])
  return <div id="jquery" />
}

export default JQuery

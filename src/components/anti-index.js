import React from "react"
import { Helmet } from "react-helmet"

export const AntiIndex = () => {
  return (
    <Helmet defer={false}>
      <meta name="robots" content="noindex" />
      <meta name="googlebot" content="noindex" />
    </Helmet>
  )
}

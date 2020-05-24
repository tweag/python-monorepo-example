import React from "react"
import { Link } from "gatsby"

import Layout from "../components/layout"
import SEO from "../components/seo"

const NotFoundPage = ({ data, location }) => {
  return (
    <Layout>
      <SEO title="Page not found" pathname="/404.html" />
      <div
        className="section s_white section-wrap viewport-section"
        style={{ paddingTop: "150px", margin: "0 auto 0 auto", width: "32em" }}
      >
        <h1>Page not found</h1>
        <p style={{ paddingTop: "1em", paddingBottom: "1em" }}>
          Oops! The page you are looking for has been removed or relocated.
        </p>
        <p>
          <Link to="/" className="btn noarrow" style={{ margin: 0 }}>
            Go back
          </Link>
        </p>
      </div>
    </Layout>
  )
}

export default NotFoundPage

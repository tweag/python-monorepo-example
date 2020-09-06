import React from "react"
import { graphql } from "gatsby"

import Layout from "../components/layout"
import { Card } from "../components/blog-card"
import SEO from "../components/seo"

const TagPage = ({ data, pageContext }) => {
  const { tag } = pageContext
  const { edges, totalCount } = data.allMarkdownRemark
  const title = `Blog: ${tag} (${totalCount} post${
    totalCount === 1 ? `` : `s`
  })`
  return (
    <Layout>
      <SEO title={`Engineering Blog: ${tag}`} />
      <section className="section-area">
        <div className="section s_white services-section blog-content blog-home viewport-section">
          <div className="text-area">
            <div className="section-title">{title}</div>
          </div>
        </div>
        <div className="section s_white services-section blog-content viewport-section">
          <div className="post_container">
            {edges.map(post => {
              return <Card node={post.node} key={post.node.fields.slug} />
            })}
          </div>
        </div>
      </section>
    </Layout>
  )
}

export default TagPage

export const pageQuery = graphql`
  query($tag: String) {
    allMarkdownRemark(
      sort: { fields: [fields___slug], order: DESC }
      filter: { frontmatter: { tags: { in: [$tag] } } }
    ) {
      totalCount
      edges {
        node {
          excerpt(pruneLength: 280)
          fields {
            date(formatString: "D MMMM YYYY")
            slug
          }
          frontmatter {
            title
            shortTitle
            description
            tags
          }
        }
      }
    }
  }
`

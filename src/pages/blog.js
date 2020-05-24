import React from "react"
import { graphql } from "gatsby"

import Layout from "../components/layout"
import { Card, TopCard } from "../components/blog-card"
import SEO from "../components/seo"

const BlogIndex = ({ data }) => {
  const topPost = data.allMarkdownRemark.edges[0]
  const posts = data.allMarkdownRemark.edges.slice(1)
  return (
    <Layout>
      <SEO title="Engineering Blog" />
      <section className="section-area">
        <div className="services-section Blog-content Blog-home viewport-section">
          <div className="text-area">
            <div className="section-title">Blog</div>
          </div>
        </div>
        <TopCard node={topPost.node} />
        <div className="services-section Blog-content viewport-section">
          <div className="post_container">
            {posts.map(post => {
              return <Card node={post.node} />
            })}
          </div>
        </div>
      </section>
    </Layout>
  )
}

export default BlogIndex

export const pageQuery = graphql`
  query {
    allMarkdownRemark(sort: { fields: [fields___slug], order: DESC }) {
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

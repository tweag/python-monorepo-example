import React from "react"

import Layout from "../components/layout"
import { Card, TopCard } from "../components/blog-card"
import SEO from "../components/seo"
import AllTags from "../components/all-tags"

import pattern1 from "../images/post_pattern1.png"
import pattern2 from "../images/post_pattern2.png"
import pattern3 from "../images/post_pattern3.png"

const BlogIndex = ({ data }) => {
  const topPost = data.allMarkdownRemark.edges[0]
  const posts = data.allMarkdownRemark.edges.slice(1)
  const patterns = [pattern1, pattern2, pattern3]
  return (
    <Layout>
      <SEO title="Engineering Blog" />
      <section className="section s_white section-area">
        <div className="services-section blog-content blog-home viewport-section">
          <div className="text-area">
            <div className="section-title" style={{ marginBottom: `20px` }}>
              Blog
            </div>
            <AllTags />
          </div>
        </div>
        <TopCard node={topPost.node} />
        <div className="services-section blog-content viewport-section">
          <div className="post_container">
            {posts.map((post, i) => {
              // Display a pattern periodically.
              const period = 6
              const start = 3
              const pattern = patterns[((i + start) / period) % patterns.length]
              if ((i + start) % period === 0) {
                return (
                  <>
                    <div className="post_content notop">
                      <img src={pattern} alt="" />
                    </div>
                    <Card node={post.node} />
                  </>
                )
              } else {
                return <Card node={post.node} />
              }
            })}
          </div>
        </div>
      </section>
    </Layout>
  )
}

export default BlogIndex

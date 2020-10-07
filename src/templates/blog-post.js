import React from "react"
import { Link, graphql } from "gatsby"

import Layout from "../components/layout"
import SEO from "../components/seo"

import "katex/dist/katex.min.css"

import cc from "../images/cc.svg"
import ccBy from "../images/cc-by.svg"

const BlogPostTemplate = ({ data, pageContext }) => {
  const post = data.markdownRemark
  const { previous, next } = pageContext
  const prevTitle =
    previous && (previous.frontmatter.shortTitle || previous.frontmatter.title)
  const nextTitle =
    next && (next.frontmatter.shortTitle || next.frontmatter.title)

  return (
    <Layout>
      <SEO
        title={post.frontmatter.title}
        description={post.frontmatter.description || post.excerpt}
      />
      <article className="section s_white section-area">
        <header className="services-section opensource1 blog-content viewport-section">
          <div className="text-area">
            <div className="section-title">
              {post.fields.date} — by {post.frontmatter.author}
            </div>
            <h1>{post.frontmatter.title}</h1>
            {post.frontmatter.tags && (
              <div className="post_tags">
                {post.frontmatter.tags.map(tag => {
                  return (
                    <Link
                      to={`/blog/tags/${tag}`}
                      key={tag}
                      className="btn noarrow"
                    >
                      {tag}
                    </Link>
                  )
                })}
              </div>
            )}
          </div>
          <div className="posts-holder image-holder">
            {/* TODO: Implement the functionality of latest posts.
            <h3>RECENT POSTS</h3>
            <ul>
              <li>
                A taste of Bazel: build a library, a service and hspec tests
              </li>
              <li>
                Lorem ipsum dolor sit amet Lorem ipsum dolor sit amet Lorem
                ipsum dolor sit amet Lorem ipsum dolor sit amet
              </li>
              <li>Lorem ipsum dolor sit amet</li>
              <li>Lorem ipsum dolor sit amet</li>
            </ul>
            */}
          </div>
        </header>
        <section className="services-section opensource1 blog-content part2 viewport-section in-viewport">
          <div className="text-area">
            <div className="text-wrap">
              <div
                className="article-text"
                dangerouslySetInnerHTML={{
                  __html: post.html,
                }}
              />
            </div>
          </div>
          <div className="licence view">
            <img className="cc-icon" src={cc} />
            <img className="cc-icon" src={ccBy} />
            This article is licensed under a{` `}
            <a href="https://creativecommons.org/licenses/by/4.0/">
              Creative Commons Attribution 4.0 International
            </a>
            {` `}
            license.
          </div>
        </section>
      </article>
      <nav>
        <ul
          style={{
            display: `flex`,
            flexWrap: `wrap`,
            justifyContent: `space-between`,
            listStyle: `none`,
            padding: 0,
          }}
        >
          <li>
            {previous && (
              <Link to={previous.fields.slug} rel="prev">
                ← {prevTitle}
              </Link>
            )}
          </li>
          <li>
            {next && (
              <Link to={next.fields.slug} rel="next">
                {nextTitle} →
              </Link>
            )}
          </li>
        </ul>
      </nav>
    </Layout>
  )
}

export default BlogPostTemplate

export const pageQuery = graphql`
  query BlogPostBySlug($slug: String!) {
    markdownRemark(fields: { slug: { eq: $slug } }) {
      id
      html
      fields {
        date(formatString: "D MMMM YYYY")
        slug
      }
      frontmatter {
        title
        shortTitle
        author
        description
        tags
      }
    }
  }
`

import React from "react"
import { Link } from "gatsby"

import homePost from "../images/home_post.png"

export const TopCard = ({ node }) => {
  const title = node.frontmatter.shortTitle || node.frontmatter.title
  const tags = node.frontmatter.tags
  return (
    <div className="services-section Blog-content Blog-home topsep viewport-section">
      <article className="text-area">
        <Link
          to={node.fields.slug}
          className="post_content_1"
          // TODO put in class
          style={{ color: `#000` }}
        >
          <header>
            <div className="section-title homepost">{node.fields.date}</div>
            <h1>{title}</h1>
          </header>
          <section>
            <p
              dangerouslySetInnerHTML={{
                __html: node.frontmatter.description || node.excerpt,
              }}
            />
          </section>
        </Link>
        <div className="post_content_2">
          {tags && (
            <div className="post_tags">
              {tags.map(tag => {
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
      </article>
      <div className="home-holder highlight image-holder">
        <img src={homePost} alt="" />
      </div>
    </div>
  )
}

export const Card = ({ node }) => {
  const title = node.frontmatter.shortTitle || node.frontmatter.title
  const tags = node.frontmatter.tags
  return (
    <article className="post_content" key={node.fields.slug}>
      <Link
        to={node.fields.slug}
        className="post_content_1"
        style={{ color: `#000` }}
      >
        <header>
          <div className="post_date">{node.fields.date}</div>
          <div className="post_title">
            <h2>{title}</h2>
          </div>
        </header>
        <section className="post_excerpt">
          <p
            dangerouslySetInnerHTML={{
              __html: node.frontmatter.description || node.excerpt,
            }}
          />
        </section>
      </Link>
      <div className="post_content_2">
        {tags && (
          <div className="post_tags">
            {tags.map(tag => {
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
    </article>
  )
}

/** @jsx jsx */
import { Link, graphql } from "gatsby"
import { jsx, Flex, Text, Box } from "theme-ui"
import "katex/dist/katex.min.css"

import { DefaulLayout as Layout } from "../layouts"
import { SEO, SectionHeading, Tags, BlogPostContent } from "../components"
import cc from "../images/cc.svg"
import ccBy from "../images/cc-by.svg"

const BlogPostTemplate = ({ data, pageContext }) => {
  const post = data.markdownRemark
  const { previous, next } = pageContext
  const prevTitle =
    previous && (previous.frontmatter.shortTitle || previous.frontmatter.title)
  const nextTitle =
    next && (next.frontmatter.shortTitle || next.frontmatter.title)

  const allTags = (post.frontmatter.tags || []).map(tag => ({
    tag,
    link: `/blog/tags/${tag}`,
  }))

  return (
    <Layout>
      <SEO
        title={post.frontmatter.title}
        description={post.frontmatter.description || post.excerpt}
      />
      <div
        className="section s_white transition-section viewport-section"
        sx={{
          pt: [`65px`, `65px`, `130px`, `130px`, `130px`, `130px`, `160px`],
        }}
      >
        <Flex
          className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed-0"
          sx={{
            flexDirection: `column`,
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            width: [`100%`, `100%`, `65%`, `55%`, `55%`, `55%`, `70%`],
          }}
        >
          <SectionHeading
            customSx={{
              width: `fit-content`,
            }}
          >
            {post.fields.date} — by {post.frontmatter.author}
          </SectionHeading>
          <Text
            sx={{
              mt: [`45px`],
              mb: [`35px`],
              fontSize: [`34px`, `34px`, `66px`],
              lineHeight: [1],
              fontWeight: 700,
              textTransform: `uppercase`,
              minHeight: `100px`,
            }}
          >
            {post.frontmatter.title}
          </Text>
          <Tags tags={allTags} />
        </Flex>
        <Box
          sx={{
            mt: [`20px`],
          }}
        >
          <BlogPostContent
            dangerouslySetInnerHTML={{
              __html: post.html,
            }}
          />
        </Box>
        <Box
          sx={{
            mt: [`40px`],
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            fontWeight: 700,
          }}
        >
          If you enjoyed this article, you might be interested in {` `}
          <a href="/careers">joining the Tweag team</a>.
        </Box>
        <Box
          sx={{
            mt: [`40px`],
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          }}
        >
          <img
            sx={{ display: `inline-block`, height: `1em`, mr: [`8px`] }}
            className="cc-icon"
            src={cc}
          />
          <img
            sx={{ display: `inline-block`, height: `1em`, mr: [`8px`] }}
            className="cc-icon"
            src={ccBy}
          />
          This article is licensed under a{` `}
          <a href="https://creativecommons.org/licenses/by/4.0/">
            Creative Commons Attribution 4.0 International
          </a>
          {` `}
          license.
        </Box>
        <Flex
          sx={{
            justifyContent: [`space-between`],
            flexWrap: `wrap`,
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            pr: [`15px`, `15px`, 0],
            mt: [`30px`],
            mb: [`30px`],
            width: [`100%`, `100%`, `65%`],
            maxWidth: `1000px`,
          }}
        >
          {previous && (
            <Link sx={{ color: `#000` }} to={previous.fields.slug} rel="prev">
              ← {prevTitle}
            </Link>
          )}
          {next && (
            <Link sx={{ color: `#000` }} to={next.fields.slug} rel="next">
              {nextTitle} →
            </Link>
          )}
        </Flex>
      </div>
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

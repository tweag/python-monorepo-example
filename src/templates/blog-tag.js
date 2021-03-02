/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid } from "theme-ui"
import { graphql } from "gatsby"

import { Layout, SectionHeading, Tags, SEO } from "../components"
import BlogCard from "../components/blog-card"

import { useAllTags } from "../hooks"

const TagPage = ({ data, pageContext }) => {
  const allTags = useAllTags({ withLinks: true, appendGeneral: `all` })
  const { tag } = pageContext
  const { edges, totalCount } = data.allMarkdownRemark
  const title = `Blog: ${tag} (${totalCount} post${
    totalCount === 1 ? `` : `s`
  })`

  return (
    <Layout>
      <SEO title={`Engineering Blog ${tag}`} />
      <div
        className="section s_white  viewport-section transition-section"
        sx={{
          pt: [`65px`, `65px`, `130px`, `130px`, `130px`, `130px`, `160px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed-0"
          gap={`20px`}
          sx={{
            px: [`15px`, `15px`, 0],
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            width: [`100%`, `100%`, `50%`],
          }}
        >
          <SectionHeading
            customSx={{
              justifySelf: `start`,
            }}
          >
            {title}
          </SectionHeading>
          <Tags tags={allTags} />
        </Grid>
        <Grid
          sx={{
            mt: [`20px`],
            mb: [`40px`],
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            // pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            rowGap: [`60px`, `60px`, `40px`],
            columnGap: [0, 0, `30px`, `50px`, `5%`],
          }}
          columns={[1, 1, 3]}
          // gap={[`60px`, `60px`, 0]}
        >
          {edges.map((post, i) => {
            // Display a pattern periodically.
            return <BlogCard key={i} node={post.node} />
          })}
        </Grid>
      </div>
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

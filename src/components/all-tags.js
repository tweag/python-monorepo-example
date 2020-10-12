import React from "react"
import { graphql, StaticQuery, Link } from "gatsby"

// Static queries are used for queries in non page components. For more info look at:
// https://www.gatsbyjs.com/docs/static-query/

const AllTagsByData = ({ data }) => {
  const posts = data.allMarkdownRemark.edges.slice(1)
  const tagsNumCitetions = {}
  posts.forEach(post => {
    post.node.frontmatter.tags.forEach(t => {
      if (tagsNumCitetions[t] == null) tagsNumCitetions[t] = 0
      else tagsNumCitetions[t] += 1
    })
  })
  const tagsUniques = Object.keys(tagsNumCitetions).sort(
    (a, b) => tagsNumCitetions[b] - tagsNumCitetions[a]
  )
  return (
    <div
      className="all_tags"
      style={{
        marginBottom: `20px`,
      }}
    >
      <div>
        <Link to={`/blog`} key="all" className="btn noarrow">
          all
        </Link>
        {tagsUniques.map(tag => {
          return (
            <Link to={`/blog/tags/${tag}`} key={tag} className="btn noarrow">
              {tag}
            </Link>
          )
        })}
      </div>
    </div>
  )
}

const query = graphql`
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

const AllTags = () => (
  <StaticQuery query={query} render={data => <AllTagsByData data={data} />} />
)

export default AllTags

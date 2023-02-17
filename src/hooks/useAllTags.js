import { graphql, useStaticQuery } from "gatsby"

const useAllTags = ({ withLinks = false, appendGeneral = `` }) => {
  const data = useStaticQuery(graphql`
    query {
      allMarkdownRemark(
        filter: { frontmatter: { key: { ne: "group" } } }
        sort: { fields: [fields___slug], order: DESC }
      ) {
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
  `)

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

  if (withLinks) {
    return [
      ...(appendGeneral && [{ link: `/blog`, tag: appendGeneral }]),
      ...tagsUniques.map(t => ({ tag: t, link: `/blog/tags/${t}` })),
    ]
  }

  if (appendGeneral) {
    return [appendGeneral, ...tagsUniques]
  }

  return tagsUniques
}

export default useAllTags

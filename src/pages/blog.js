import { graphql } from "gatsby"

import Blog from "../views/blog"

export default Blog

export const query = graphql`
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
`

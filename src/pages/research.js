import { graphql } from "gatsby"
import Research from "./../views/research"

export const query = graphql`
  query QueryArticles {
    allArticlesYaml {
      edges {
        node {
          abstract
          authors {
            name
            tweag
          }
          date
          links
          pdf
          status
          tags
          title
        }
      }
    }
    allFile(filter: { extension: { eq: "pdf" } }) {
      edges {
        node {
          name
          publicURL
        }
      }
    }
  }
`

export default Research

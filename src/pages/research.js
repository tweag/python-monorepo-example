import { graphql } from "gatsby"
import Research from "./../views/research"

export const query = graphql`
  query QueryPapers {
    file(
      sourceInstanceName: { eq: "papers" }
      name: { eq: "index" }
      extension: { eq: "yaml" }
    ) {
      internal {
        content
      }
    }
    allFile(
      filter: {
        sourceInstanceName: { eq: "papers" }
        extension: { ne: "yaml" }
      }
    ) {
      edges {
        node {
          publicURL
          name
          extension
        }
      }
    }
  }
`
export default Research

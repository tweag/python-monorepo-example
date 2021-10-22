import { graphql } from "gatsby"
import Team from "./../views/team"

export const query = graphql`
  query QueryProfiles {
    profileImages: allFile(
      filter: { sourceInstanceName: { eq: "profilePictures" } }
    ) {
      edges {
        node {
          name
          publicURL
          children {
            ... on ImageSharp {
              id
              fluid {
                srcWebp
              }
            }
          }
        }
      }
    }
    profiles: allFile(filter: { sourceInstanceName: { eq: "profiles" } }) {
      edges {
        node {
          name
          internal {
            content
          }
        }
      }
    }
  }
`

export default Team

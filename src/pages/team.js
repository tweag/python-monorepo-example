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
              fixed(
                height: 300
                width: 300
                cropFocus: NORTH
                toFormat: WEBP
                webpQuality: 85
              ) {
                src
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

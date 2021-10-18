import { graphql } from "gatsby"
import Team from "./../views/team"

export const query = graphql`
  query QueryProfiles {
    allFile(
      filter: {
        extension: { eq: "jpg" }
        sourceInstanceName: { eq: "profilePictures" }
      }
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
    allProfilesYaml {
      edges {
        node {
          bio
          experience {
            employer
            role
          }
          name
          skills
          slug
        }
      }
    }
  }
`

export default Team

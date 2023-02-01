import { css } from "@emotion/react"
import { graphql } from "gatsby"
import React from "react"
import { Box, Flex, Grid, Text } from "theme-ui"
import { BlogPostContent, SectionHeading, SEO } from "../components"
import Layout from "../layouts/default-page"

export const pageQuery = graphql`
  query GroupBySlug($slug: String!, $members: [String]) {
    markdownRemark(fields: { slug: { eq: $slug } }) {
      html
      fields {
        slug
      }
      frontmatter {
        title
      }
      members {
        slug
        name
      }
    }
    profileImages: allFile(
      filter: {
        sourceInstanceName: { eq: "profilePictures" }
        name: { in: $members }
      }
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
  }
`

type MemberWithPicture = {
  picture?: string
} & Member

const MemberList: React.FC<{ members: MemberWithPicture[] }> = ({
  members,
}) => (
  <Flex
    sx={{
      gap: `1rem`,
      width: `100%`,
      flexWrap: `wrap`,
      mt: [`20px`],
    }}
  >
    {members.map((member, index) => (
      <Flex
        key={index}
        sx={{
          width: `100px`,
          flexDirection: `column`,
          justifyContent: `space-between`,
          alignItems: `center`,
        }}
      >
        <img
          src={member.picture}
          alt={member.name}
          style={{ width: `100px`, borderRadius: `8%` }}
        />
        <Text
          as="div"
          sx={{
            fontSize: `1rem`,
            lineHeight: [1],
            fontWeight: 700,
            textTransform: `uppercase`,
            mt: [`10px`],
            mb: [`10px`],
          }}
        >
          {member.name}
        </Text>
      </Flex>
    ))}
  </Flex>
)

type Member = {
  name: string
  slug: string
}

type Group = {
  html: string
  fields: {
    slug: string
  }
  frontmatter: {
    title: string
  }
  members: Member[]
}

type Props = {
  data: {
    markdownRemark: Group
    profileImages: {
      edges: Array<{
        node: {
          name: string
          publicURL: string
        }
      }>
    }
  }
}

const GroupTemplate: React.FC<Props> = ({ data }) => {
  const group = data.markdownRemark
  console.log({ images: data.profileImages })
  const membersWithPicture = group.members.map(member => ({
    ...member,
    picture: data.profileImages.edges.find(
      picture => picture.node.name === member.slug
    )?.node.publicURL,
  }))

  return (
    <Layout>
      <SEO title={group.frontmatter.title} pathname={group.fields.slug} />

      <Grid
        className="section s_white  viewport-section transition-section"
        columns={[2, `5fr 2fr`]}
        sx={{
          px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          pt: [`60px`, `60px`, `130px`],
        }}
      >
        <Grid gap={`35px`}>
          <Text
            className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed-0"
            as="div"
            sx={{
              mt: [`45px`],
              px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
              mb: [`35px`],
              fontSize: [`34px`, `34px`, `66px`],
              lineHeight: [1],
              fontWeight: 700,
              textTransform: `uppercase`,
              minHeight: `100px`,
            }}
          >
            {group.frontmatter.title}
          </Text>
          <Box sx={{ mt: [`20px`] }}>
            <BlogPostContent
              dangerouslySetInnerHTML={{
                __html: group.html,
              }}
            />
          </Box>
        </Grid>
        <Grid
          css={css`
            animation: slowFadeIn 2s linear;
            animation-fill-mode: backwards;

            @keyframes slowFadeIn {
              from {
                opacity: 0;
              }
              to {
                opacity: 1;
              }
            }
          `}
          columns={1}
        >
          <div>
            <SectionHeading
              customSx={{
                width: `fit-content`,
                display: `flex`,
                alignItems: `flex-end`,
              }}
            >
              Members
            </SectionHeading>
            <MemberList members={membersWithPicture} />
          </div>
        </Grid>
      </Grid>
    </Layout>
  )
}

export default GroupTemplate

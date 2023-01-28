import { graphql } from "gatsby"
import React from "react"
import { Box, Flex, Grid, Text } from "theme-ui"
import { BlogPostContent, SectionHeading, SEO } from "../components"
import Layout from "../layouts/default-page"

export const pageQuery = graphql`
  query GroupBySlug($slug: String!) {
    markdownRemark(fields: { slug: { eq: $slug } }) {
      html
      fields {
        slug
      }
      frontmatter {
        title
      }
      members {
        name
      }
    }
  }
`

const MemberList: React.FC<{ members: Array<{ name: string }> }> = ({
  members,
}) => (
  <Flex sx={{ flexDirection: `column` }}>
    {members.map((member, index) => (
      <Text
        key={index}
        as="div"
        sx={{
          fontSize: [`24px`, `24px`, `34px`, `34px`, `34px`, `34px`, `42px`],
          lineHeight: [1],
          fontWeight: 700,
          textTransform: `uppercase`,
          mt: [`10px`],
          mb: [`10px`],
        }}
      >
        {member.name}
      </Text>
    ))}
  </Flex>
)

type Group = {
  html: string
  fields: {
    slug: string
  }
  frontmatter: {
    title: string
  }
  members: Array<{
    name: string
  }>
}

type Props = {
  data: {
    markdownRemark: Group
  }
}

const GroupTemplate: React.FC<Props> = ({ data }) => {
  const group = data.markdownRemark

  return (
    <Layout>
      <SEO title={group.frontmatter.title} pathname={group.fields.slug} />

      <Grid
        className="section s_white  viewport-section transition-section"
        columns={[2, "5fr 1fr"]}
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
        <Grid columns={1}>
          <SectionHeading
            customSx={{
              width: `fit-content`,
              display: `flex`,
              alignItems: `flex-end`,
            }}
          >
            Members
          </SectionHeading>
          <MemberList members={group.members} />
        </Grid>
      </Grid>
    </Layout>
  )
}

export default GroupTemplate

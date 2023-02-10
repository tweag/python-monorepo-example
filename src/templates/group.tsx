import { css } from "@emotion/react"
import { graphql } from "gatsby"
import React from "react"
import { Box, Flex, Grid, Text } from "theme-ui"
import {
  BlogCard,
  BlogPostContent,
  Divider,
  SectionHeading,
  SEO,
} from "../components"
import Layout from "../layouts/default-page"

export const pageQuery = graphql`
  query GroupBySlug($slug: String!, $members: [String], $tags: [String]) {
    markdownRemark(fields: { slug: { eq: $slug } }) {
      html
      fields {
        slug
      }
      frontmatter {
        title
        resources {
          name
          link
        }
        members {
          slug
          role
        }
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
    articles: allMarkdownRemark(
      filter: { frontmatter: { key: { ne: "group" }, tags: { in: $tags } } }
      limit: 6
      sort: { fields: fields___date, order: DESC }
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

type Group = {
  html: string
  fields: {
    slug: string
  }
  frontmatter: {
    title: string
    resources: Resource[]
    members: Array<{
      slug: string
      role?: string
    }>
  }
  members: Member[]
}

type Resource = {
  name: string
  link: string
}

type Member = {
  name: string
  slug: string
}

type ArticleEdge = {
  node: Article
}

type Article = {
  excerpt: string
  fields: {
    date: string
    slug: string
  }
  frontmatter: {
    title: string
    shortTitle: string
    description: string
    tags: string[]
  }
}

type FullMember = {
  picture?: string
  role?: string
} & Member

const Description: React.FC<{ group: Group }> = ({ group }) => (
  <Grid
    gap={`35px`}
    className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed-0"
  >
    <SectionHeading customSx={{ width: `fit-content` }}>
      Group profile
    </SectionHeading>
    <Box
      as="div"
      sx={{
        px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
        fontSize: [`34px`, `34px`, `66px`],
        lineHeight: [1],
        fontWeight: 700,
        textTransform: `uppercase`,
      }}
    >
      {group.frontmatter.title}
    </Box>
    <Box>
      <BlogPostContent
        dangerouslySetInnerHTML={{
          __html: group.html,
        }}
      />
    </Box>
  </Grid>
)

const Resources: React.FC<{ resources: Resource[] }> = ({ resources }) => (
  <Grid
    gap={[`25px`]}
    className="transition-section__transition--slide-fade-in bottom-in only-above-1"
  >
    <SectionHeading customSx={{ width: `fit-content` }}>
      Resources
    </SectionHeading>
    <Grid gap={[`30px`]}>
      {resources.map((resource, index) => (
        <Box
          key={index}
          sx={{
            gap: `1rem`,
            fontWeight: [700],
            fontSize: [`18px`, `18px`, `34px`],
            lineHeight: [`22px`, `22px`, 1.1],
          }}
        >
          <i className="icon-arrow-right1"></i>
          <a href={resource.link} target="_blank" rel="noreferrer">
            {resource.name}
          </a>
        </Box>
      ))}
    </Grid>
  </Grid>
)

const RelatedArticles: React.FC<{ edges: ArticleEdge[] }> = ({ edges }) => {
  if (edges.length === 0) {
    return <></>
  }

  return (
    <Grid>
      <Divider
        customSx={{
          mx: [`20px`, `20p`, `60px`],
          my: [`60px`],
        }}
      />
      <Text
        as="div"
        sx={{
          fontSize: [`24px`, `24px`, `34px`, `34px`, `34px`, `34px`, `42px`],
          lineHeight: [1],
          fontWeight: [700],
          textTransform: `uppercase`,
          textAlign: `center`,
        }}
      >
        Articles from this team on our blog
      </Text>
      <Grid
        sx={{
          mt: [`40px`],
          mb: [`40px`],
          rowGap: [`60px`, `60px`, `40px`],
          columnGap: [0, 0, `30px`, `50px`, `5%`],
        }}
        columns={[1, 1, 3]}
      >
        {edges.map(({ node }, i) => (
          <BlogCard key={i} node={node} />
        ))}
      </Grid>
    </Grid>
  )
}

const MemberList: React.FC<{ members: FullMember[] }> = ({ members }) => (
  <Grid
    className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed-0"
    columns={1}
  >
    <SectionHeading
      customSx={{
        width: `fit-content`,
        display: `flex`,
        alignItems: `flex-end`,
      }}
    >
      Members
    </SectionHeading>
    <Flex
      sx={{
        gap: `1rem`,
        width: `100%`,
        flexWrap: `wrap`,
        mt: [`20px`],
      }}
    >
      {members.map((member, index) => (
        <MemberCard key={index} member={member} />
      ))}
    </Flex>
  </Grid>
)

const memberRoleStyles = (role: string) => ({
  imgWrapper: css`
    &::before {
      content: "${role}";
      position: absolute;
      font-size: 0.75rem;
      color: white;
      top: -0.75rem;
      width: 100%;
      display: block;
      background: var(--theme-ui-colors-blue);
      text-align: center;
      text-transform: capitalize;
      border-radius: 8px 8px 0 0;
    }
  `,
  img: {
    border: `2px solid var(--theme-ui-colors-blue)`,
    borderRadius: `0 0 8px 8px`,
  },
})
const MemberCard: React.FC<{ member: FullMember }> = ({ member }) => {
  const roleStyle = member.role
    ? memberRoleStyles(member.role)
    : {
        imgWrapper: css``,
        img: {
          borderRadius: `8%`,
        },
      }

  return (
    <Flex
      sx={{
        width: `100px`,
        position: `relative`,
        flexDirection: `column`,
        justifyContent: `space-between`,
        alignItems: `center`,
        gap: `0.5rem`,
      }}
    >
      <Box css={roleStyle.imgWrapper}>
        <img
          src={
            member.picture || `https://placehold.co/100x150?text=No+Picture}`
          }
          alt={member.name}
          style={{
            width: `100px`,
            height: `150px`,
            objectFit: `cover`,
            position: `relative`,
            ...roleStyle.img,
          }}
        />
      </Box>
      <Box
        as="div"
        sx={{
          height: `100%`,
          fontSize: `1rem`,
          lineHeight: [1],
          fontWeight: 700,
          textTransform: `uppercase`,
          alignItems: `flex-start`,
        }}
      >
        {member.name}
      </Box>
    </Flex>
  )
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
    articles: {
      edges: ArticleEdge[]
    }
  }
}

const GroupTemplate: React.FC<Props> = ({ data }) => {
  const group = data.markdownRemark

  const membersWithPicture = group.members.map(member => ({
    ...member,
    picture: data.profileImages.edges.find(
      picture => picture.node.name === member.slug
    )?.node.publicURL,
    role:
      group.frontmatter.members.find(m => m.slug === member.slug)?.role || ``,
  }))

  return (
    <Layout>
      <SEO title={group.frontmatter.title} pathname={group.fields.slug} />

      <Grid
        className="section s_white  viewport-section transition-section"
        columns={1}
        gap={`5rem`}
        sx={{
          px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          pt: [`60px`, `60px`, `130px`],
        }}
      >
        <Description group={group} />
        <Resources resources={group.frontmatter.resources} />
        <MemberList members={membersWithPicture} />
        <RelatedArticles edges={data.articles.edges} />
      </Grid>
    </Layout>
  )
}

export default GroupTemplate

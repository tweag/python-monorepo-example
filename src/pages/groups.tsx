import React from "react"
import { Flex, Grid, Text } from "theme-ui"
import { SectionHeading, SEO } from "../components"
import Layout from "../layouts/default-page"
import img14 from "../images/img14.svg"
import { graphql, Link } from "gatsby"

export const query = graphql`
  query {
    allMarkdownRemark(filter: { frontmatter: { key: { eq: "group" } } }) {
      edges {
        node {
          fields {
            slug
          }
          frontmatter {
            title
            description
            slug
          }
        }
      }
    }
  }
`

type Group = {
  fields: {
    slug: string
  }
  frontmatter: {
    title: string
    description: string
    slug: string
  }
}

type GroupGridProps = {
  edges: Edge[]
}

const GroupGrid: React.FC<GroupGridProps> = ({ edges }) => (
  <Grid
    sx={{
      mt: [`80px`],
      mb: [`40px`],
      rowGap: [`60px`, `60px`, `40px`],
      columnGap: [0, 0, `30px`, `50px`, `5%`],
    }}
    columns={[1, 1, 3]}
  >
    {edges.map(({ node }, i) => (
      <GroupCard key={i} group={node} />
    ))}
  </Grid>
)

const GroupCard: React.FC<{ group: Group }> = ({ group }) => (
  <Flex
    sx={{
      borderTop: `solid 1px black;`,
      gridAutoRows: `max-content`,
      flexDirection: `column`,
      justifyContent: `space-between`,
      minHeight: [`auto`, `auto`, `350px`],
      "&:hover": {
        transform: `scale(1.05)`,
      },
      transition: `transform 0.2s ease-in-out`,
    }}
  >
    <Link to={group.fields.slug} style={{ color: `#000` }}>
      <Flex sx={{ flexDirection: `column` }}>
        <Text
          as="div"
          sx={{
            fontSize: [`24px`, `24px`, `34px`, `34px`, `34px`, `34px`, `42px`],
            lineHeight: [1],
            fontWeight: 700,
            textTransform: `uppercase`,
            mt: [`10px`],
            display: `-webkit-box`,
            "-webkit-line-clamp": `3`,
            "-webkit-box-orient": `vertical`,
            overflow: `hidden`,
          }}
        >
          {group.frontmatter.title}
        </Text>
        <Text
          as="div"
          sx={{
            fontSize: [`16px`],
            mt: [`20px`],
            display: `-webkit-box`,
            "-webkit-line-clamp": `4`,
            "-webkit-box-orient": `vertical`,
            overflow: `hidden`,
          }}
        >
          {group.frontmatter.description}
        </Text>
      </Flex>
    </Link>
  </Flex>
)

type Edge = {
  node: Group
}

type Props = {
  data: {
    allMarkdownRemark: {
      edges: Edge[]
    }
  }
}

const Groups: React.FC<Props> = ({ data }) => {
  const { edges } = data.allMarkdownRemark

  return (
    <Layout>
      <SEO title="Groups" pathname="/groups" />

      <Grid
        className="section s_white  viewport-section transition-section"
        columns={1}
        gap={[`35px`, `35px`]}
        sx={{
          px: [`15px`, `15px`, `0px`],
          pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          pt: [`60px`, `60px`, `130px`],
        }}
      >
        <SectionHeading customSx={{ width: `fit-content` }}>
          Our Technical Groups
        </SectionHeading>

        <Grid columns={2}>
          <Grid columns={1} gap="35px">
            <Text
              as="div"
              sx={{
                textTransform: `uppercase`,
                fontSize: [`34px`, `34px`, `66px`],
                fontWeight: 700,
                lineHeight: [1, 1],
                mt: [`20px`, `20px`, `50px`],
              }}
            >
              Organising around a problem space
            </Text>
            <Text
              as="div"
              sx={{
                fontSize: [`18px`, `18px`, `27px`],
                lineHeight: [1.2, 1.2, `35px`],
              }}
            >
              When a number of Tweag engineers work in a particular problem
              space that has clear client interest and wide potential for
              impact, a Technical Group is formed
            </Text>
          </Grid>
          <Flex
            className="transition-section__transition--slide-fade-in right-in only-above-1"
            sx={{ justifyContent: `flex-end` }}
          >
            <img src={img14} />
          </Flex>
        </Grid>
        <GroupGrid edges={edges} />
      </Grid>
    </Layout>
  )
}

export default Groups

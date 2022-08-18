/** @jsx jsx */
import { jsx, Grid, Flex, Text } from "theme-ui"
import { Link } from "gatsby"

export const TopCard = ({ node }) => {
  const title = node.frontmatter.shortTitle || node.frontmatter.title
  const tags = node.frontmatter.tags
  return (
    <Grid
      sx={{
        borderTop: [`none`, `solid 1px black`],
      }}
      gap={0}
    >
      <Link
        sx={{
          color: `#000`,
        }}
        to={node.fields.slug}
      >
        <Text
          as="div"
          sx={{
            fontSize: [`14px`],
            mt: [0, `15px`],
          }}
        >
          {node.fields.date}
        </Text>
        <Text
          as="div"
          sx={{
            fontSize: [`34px`, `66px`],
            lineHeight: [1],
            fontWeight: 700,
            textTransform: `uppercase`,
            mt: [`20px`, `30px`, `10px`],
            minHeight: [`100px`],
          }}
        >
          {title}
        </Text>
        <Text
          as="div"
          sx={{
            fontSize: [`27px`, `27px`, `18px`],
            mt: [`35px`],
          }}
          dangerouslySetInnerHTML={{
            __html: node.frontmatter.description || node.excerpt,
          }}
        />
      </Link>

      <Flex
        sx={{
          mt: [`20px`],
        }}
      >
        {tags.map(tag => {
          return (
            <Link
              to={`/blog/tags/${tag}`}
              key={tag}
              className="button button-secondary button-small"
              sx={{
                mr: [`15px`],
                mb: [`5px`],
              }}
            >
              {tag}
            </Link>
          )
        })}
      </Flex>
    </Grid>
  )
}

const BlogCard = ({ node }) => {
  const title = node.frontmatter.shortTitle || node.frontmatter.title
  const tags = node.frontmatter.tags
  return (
    <Flex
      gap={[`0px`]}
      sx={{
        borderTop: `solid 1px black;`,
        gridAutoRows: `max-content`,
        flexDirection: `column`,
        justifyContent: `space-between`,
        minHeight: [`auto`, `350px`],
      }}
    >
      <Link to={node.fields.slug} sx={{ color: `#000` }}>
        <Flex sx={{ flexDirection: `column` }}>
          <Text
            as="div"
            sx={{
              fontSize: [`16px`],
              mt: [`15px`],
            }}
          >
            {node.fields.date}
          </Text>
          <Text
            as="div"
            sx={{
              fontSize: [`24px`, `34px`, `42px`],
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
            {title}
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
            dangerouslySetInnerHTML={{
              __html: node.frontmatter.description || node.excerpt,
            }}
          />
        </Flex>
      </Link>
      <Flex
        sx={{
          mt: [`36px`],
          flexWrap: `wrap`,
        }}
      >
        {tags.map(tag => {
          return (
            <Link
              to={`/blog/tags/${tag}`}
              key={tag}
              className="button button-secondary button-small"
              sx={{
                mr: [`15px`],
                mb: [`5px`],
              }}
            >
              {tag}
            </Link>
          )
        })}
      </Flex>
    </Flex>
  )
}

BlogCard.TopCard = TopCard

export default BlogCard

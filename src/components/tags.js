/** @jsx jsx */
import { jsx, Flex } from "theme-ui"
import { Link } from "gatsby"

// Static queries are used for queries in non page components. For more info look at:
// https://www.gatsbyjs.com/docs/static-query/

/**
 * Renders tags.
 * @typedef tags
 * @property {string} tag
 * - The title of the tag
 * @property {string} link
 * - Link to the tag posts.
 *
 * @typedef Props
 * @property {tags} tags
 *
 */
const Tags = ({ tags }) => {
  return (
    <Flex
      sx={{
        flexWrap: `wrap`,
      }}
    >
      {tags.map(({ tag, link }) => {
        return (
          <Link
            to={link}
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
  )
}

export default Tags

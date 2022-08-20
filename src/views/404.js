/** @jsx jsx */
import { jsx, Grid, Text } from "theme-ui"
import { Link } from "gatsby"

import { DefaulLayout as Layout } from "../layouts"
import { SEO } from "../components"

const NotFoundPage = () => {
  return (
    <Layout>
      <SEO title="Page not found" pathname="/404.html" />
      <Grid
        sx={{
          pt: [`100px`, `100px`, `150px`],
          pb: [`60px`, `60px`, `150px`],
          px: [`15px`, `15px`],
          maxWidth: `32em`,
          margin: `auto`,
          gridAutoRows: `max-content`,
          height: [null, null, `60vh`],
        }}
        gap={[`30px`, `30px`, `10px`]}
      >
        <Text
          as="div"
          sx={{
            textTransform: `uppercase`,
            fontWeight: 700,
            fontSize: [5, 5, 8],
          }}
        >
          Page not found
        </Text>
        <Text
          as="div"
          sx={{
            fontSize: [2, 2, 2],
          }}
        >
          Oops! The page you are looking for has been removed or relocated.
        </Text>
        <Link
          sx={{
            justifySelf: `start`,
            mt: [0, 0, `20px`],
          }}
          to="/"
          className="button button-medium button-secondary"
        >
          Go back
        </Link>
      </Grid>
    </Layout>
  )
}

export default NotFoundPage

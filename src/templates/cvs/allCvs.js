/** @jsx jsx */
import { jsx, Grid, Flex } from "theme-ui"
import { Link } from "gatsby"

import { DefaulLayout as Layout } from "../../layouts"
import { SEO } from "../../components"
import md5 from "md5"

const TemplateCV1 = ({ pageContext }) => {
  const { cvs } = pageContext

  return (
    <Layout>
      <SEO title="CVs" />
      <Grid className="section s_white" sx={{ pt: `130px` }}>
        {cvs.map(({ slug, name }) => (
          <Flex key={slug} sx={{ gap: `0.5rem` }}>
            <Link to={`/cv/${slug}`}>{name}</Link>-
            <Link to={`/cv/${md5(slug)}`}>Anonymous CV</Link>
          </Flex>
        ))}
      </Grid>
    </Layout>
  )
}

export default TemplateCV1

/** @jsx jsx */
import { jsx, Grid } from "theme-ui"
import { Link } from "gatsby"

import { DefaulLayout as Layout } from "../../layouts"
import { SEO } from "../../components"

const TemplateCV1 = ({ pageContext }) => {
  const { cvs } = pageContext

  return (
    <Layout>
      <SEO title="CVs" />
      <Grid className="section s_white" sx={{ pt: `130px` }}>
        {cvs.map(({ slug, name }) => (
          <Link key={slug} to={`/cv/${slug}`}>
            {name}
          </Link>
        ))}
      </Grid>
    </Layout>
  )
}

export default TemplateCV1

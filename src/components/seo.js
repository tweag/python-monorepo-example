/**
 * SEO component that queries for data with
 *  Gatsby's useStaticQuery React hook
 *
 * See: https://www.gatsbyjs.org/docs/use-static-query/
 */

import React from "react"
import PropTypes from "prop-types"
import { Helmet } from "react-helmet"
import { useStaticQuery, graphql } from "gatsby"

const SEO = ({
  description,
  lang,
  meta = {},
  title,
  pathname,
  image = undefined,
}) => {
  const { site } = useStaticQuery(
    graphql`
      query {
        site {
          siteMetadata {
            title
            description
            social {
              twitter
            }
            siteUrl
          }
        }
      }
    `
  )

  const metaDescription = description || site.siteMetadata.description
  const canonical = pathname ? `${site.siteMetadata.siteUrl}${pathname}` : null
  // This url may be changed for SEO tests on PRs.
  // const url = `https://62e137a14014f4137dd2580e--tweag-www.netlify.app`
  const url = site.siteMetadata.siteUrl
  const ogImg = image ? `${url}${image}` : `${url}/logo.png`

  // This produces meta elements such as:
  // <meta property="og:title" content="Mapping a Universe of Open Source Software">
  // <meta name="twitter:title" content="Mapping a Universe of Open Source Software">
  const baseMetaObj = {
    "og:url": [canonical, { metaAttribute: `property` }],
    "og:type": [`article`, { metaAttribute: `property` }],
    "og:title": [title, { metaAttribute: `property` }],
    "twitter:title": [title, { metaAttribute: `name` }],
    description: [metaDescription, { metaAttribute: `name` }],
    "og:description": [description, { metaAttribute: `property` }],
    "twitter:description": [description, { metaAttribute: `name` }],
    "twitter:card": [`summary_large_image`, { metaAttribute: `name` }],
    "og:image": [ogImg, { metaAttribute: `property` }],
    "twitter:image": [ogImg, { metaAttribute: `name` }],
    "twitter:site": [
      site.siteMetadata.social.twitter,
      { metaAttribute: `name` },
    ],
    "twitter:creator": [
      site.siteMetadata.social.twitter,
      { metaAttribute: `name` },
    ],
  }

  const allMetaObj = { ...baseMetaObj, ...meta }

  const metaTags = Object.keys(allMetaObj).map(k => {
    const [value, { metaAttribute = `name` } = {}] = allMetaObj[k]
    return {
      [metaAttribute]: k,
      content: value,
    }
  })

  return (
    <Helmet
      htmlAttributes={{
        lang,
      }}
      title={title}
      titleTemplate={`%s - ${site.siteMetadata.title}`}
      link={
        canonical
          ? [
              {
                rel: `canonical`,
                href: canonical,
              },
            ]
          : []
      }
      meta={metaTags}
    />
  )
}

SEO.defaultProps = {
  lang: `en`,
  meta: [],
  description: ``,
}

SEO.propTypes = {
  description: PropTypes.string,
  lang: PropTypes.string,
  meta: PropTypes.arrayOf(PropTypes.object),
  title: PropTypes.string.isRequired,
  pathname: PropTypes.string,
}

export default SEO

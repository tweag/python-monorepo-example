module.exports = {
  siteMetadata: {
    title: `Tweag`,
    description: `
Scale your engineering power. We enable deep tech startups to achieve
their vision, from research to product delivery.
    `,
    siteUrl: `https://tweag.io`,
    social: {
      twitter: `tweagio`,
    },
  },
  plugins: [
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/content/blog`,
        name: `blog`,
      },
    },
    {
      resolve: `gatsby-source-filesystem`,
      options: {
        path: `${__dirname}/src/images`,
        name: `images`,
      },
    },
    {
      resolve: `gatsby-transformer-remark`,
      options: {
        plugins: [
          {
            resolve: `gatsby-remark-images`,
            options: {
              maxWidth: 590,
            },
          },
          {
            resolve: `gatsby-remark-responsive-iframe`,
            options: {
              wrapperStyle: `margin-bottom: 1.0725rem`,
            },
          },
          `gatsby-remark-prismjs`,
          `gatsby-remark-copy-linked-files`,
          `gatsby-remark-smartypants`,
          `gatsby-remark-katex`,
        ],
      },
    },
    `gatsby-transformer-sharp`,
    `gatsby-plugin-sharp`,
    {
      resolve: `gatsby-plugin-google-analytics`,
      options: {
        trackingId: `UA-47336061-2`,
      },
    },
    // `gatsby-plugin-feed`,
    {
      resolve: `gatsby-plugin-manifest`,
      options: {
        name: `Tweag`,
        short_name: `Tweag`,
        start_url: `/`,
        background_color: `#ffffff`,
        theme_color: `#fe7853`,
        display: `minimal-ui`,
        icon: `content/assets/tweag-icon.png`,
      },
    },
    `gatsby-plugin-react-helmet`,
    "gatsby-redirect-from",
    "gatsby-plugin-meta-redirect",
    "gatsby-plugin-sass",
    "gatsby-plugin-netlify",
    // this (optional) plugin enables Progressive Web App + Offline functionality
    // To learn more, visit: https://gatsby.dev/offline
    // `gatsby-plugin-offline`,
  ],
}

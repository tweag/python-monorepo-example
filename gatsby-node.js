/* eslint-disable @typescript-eslint/no-var-requires */

const path = require(`path`)
const _ = require(`lodash`)
const { createFilePath } = require(`gatsby-source-filesystem`)

exports.createPages = async ({ graphql, actions }) => {
  const { createPage, createRedirect } = actions

  const blogPostTemplate = path.resolve(`./src/templates/blog-post.js`)
  const groupTemplate = path.resolve(`./src/templates/group.tsx`)
  const blogTagTemplate = path.resolve(`./src/templates/blog-tag.js`)
  const cvTemplate = path.resolve(`./src/templates/cvs/template-1.js`)
  const cvListTemplate = path.resolve(`./src/templates/cvs/allCvs.js`)

  const profiles = await graphql(
    `
      {
        profiles: allProfilesYaml {
          nodes {
            slug
            name
          }
        }
      }
    `
  )

  const groupsResult = await graphql(
    `
      {
        groupsRemark: allMarkdownRemark(
          filter: { frontmatter: { key: { eq: "group" } } }
        ) {
          edges {
            node {
              fields {
                slug
              }
              frontmatter {
                title
                slug
              }
            }
          }
        }
      }
    `
  )

  const result = await graphql(
    `
      {
        postsRemark: allMarkdownRemark(
          filter: { frontmatter: { key: { ne: "group" } } }
          sort: { fields: [fields___slug], order: DESC }
          limit: 1000
        ) {
          edges {
            node {
              fields {
                date
                slug
              }
              frontmatter {
                title
              }
            }
          }
        }
        tagsGroup: allMarkdownRemark(limit: 1000) {
          group(field: frontmatter___tags) {
            fieldValue
          }
        }
      }
    `
  )

  const errors = groupsResult.errors || result.errors
  if (errors) {
    throw errors
  }

  // Create group pages.
  const groups = groupsResult.data.groupsRemark.edges
  groups.forEach(group => {
    createPage({
      path: group.node.fields.slug,
      component: groupTemplate,
      context: {
        slug: group.node.fields.slug,
      },
    })
  })

  // Create blog posts pages.
  const posts = result.data.postsRemark.edges

  posts.forEach((post, index) => {
    const previous = index === posts.length - 1 ? null : posts[index + 1].node
    const next = index === 0 ? null : posts[index - 1].node

    createPage({
      path: post.node.fields.slug,
      component: blogPostTemplate,
      context: {
        slug: post.node.fields.slug,
        previous,
        next,
      },
    })
  })

  const tags = result.data.tagsGroup.group

  tags.forEach(tag => {
    createPage({
      path: `/blog/tags/${_.kebabCase(tag.fieldValue)}/`,
      component: blogTagTemplate,
      context: {
        tag: tag.fieldValue,
      },
    })
  })

  // redirects to Bazel rule sets
  //
  // Added for the following talk at BazelCon 2021:
  // https://opensourcelive.withgoogle.com/events/bazelcon2021?talk=adding-cross-compilation-support-to-rules-haskell
  //
  // The slides for this talk contain links to these Bazel rule set
  // repositories. The conference production team unfortunately mistook them
  // for links to the Tweag website, e.g. `tweag.io/rules_haskell` instead of
  // `github.com/tweag/rules_haskell`. These links are displayed on Google's
  // conference platform next to the slides during playback.
  //
  // The following redirects ensure that these links are valid. They can be
  // dropped again at some point in the future when we feel that the likelihood
  // of someone encountering them on the BazelCon platform is low.
  createRedirect({
    fromPath: `/rules_haskell`,
    toPath: `https://github.com/tweag/rules_haskell`,
    isPermanent: true,
  })
  createRedirect({
    fromPath: `/rules_nixpkgs`,
    toPath: `https://github.com/tweag/rules_nixpkgs`,
    isPermanent: true,
  })
  createRedirect({
    fromPath: `/rules_sh`,
    toPath: `https://github.com/tweag/rules_sh`,
    isPermanent: true,
  })

  // main cvs list page;
  createPage({
    path: `/cv`,
    component: cvListTemplate,
    context: {
      cvs: profiles.data.profiles.nodes,
    },
  })

  profiles.data.profiles.nodes.forEach(({ slug }) => {
    createPage({
      path: `/cv/${slug}`,
      component: cvTemplate,
      context: {
        slug,
      },
    })
  })
  // Redirect old careers page
  createRedirect({
    fromPath: `/careers`,
    toPath: `https://boards.greenhouse.io/tweag`,
    isPermanent: true,
  })
}

exports.onCreateNode = ({ node, actions, getNode }) => {
  const { createNodeField } = actions

  if (node.internal.type !== `MarkdownRemark`) {
    return
  }

  const value = createFilePath({ node, getNode })

  let slug = `/group${value}`
  if (node.frontmatter.key !== `group`) {
    slug = `/blog${value}`
    const matches = value.match(/\d\d\d\d-\d\d-\d\d/) || [``]
    createNodeField({
      node,
      name: `date`,
      value: matches[0],
    })
  }

  createNodeField({
    node,
    name: `slug`,
    value: slug,
  })
}

exports.createSchemaCustomization = ({ actions }) => {
  const { createTypes } = actions
  const typeDefs = `
  type ProfilesYamlExperience   {
     employer: String
     role: String
     years: String
     location: String
     description: [String]
  }

  type ProfilesYamlEducation  {
    qualification: String
    name: String
    institution: String
    years: String
    description: [String]
  }

  type PublicationYaml {
    description: String
    link: String
  }

  type ProfilesYaml implements Node @dontInfer {
    slug: String!
    name: String!
    pronouns: String
    github: String
    shortDescription: String
    bio: String
    skills: [String]
    speaks: [String]
    publications: [PublicationYaml]
    experience: [ProfilesYamlExperience]
    education: [ProfilesYamlEducation]
  }

  type MarkdownRemark implements Node {
    members: [ProfilesYaml] @link(by: "slug", from: "frontmatter.members")
  }
  `
  createTypes(typeDefs)
}

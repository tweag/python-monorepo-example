const path = require(`path`)
const _ = require(`lodash`)
const { createFilePath } = require(`gatsby-source-filesystem`)

exports.createPages = async ({ graphql, actions }) => {
  const { createPage, createRedirect } = actions

  const blogPostTemplate = path.resolve(`./src/templates/blog-post.js`)
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

  const result = await graphql(
    `
      {
        postsRemark: allMarkdownRemark(
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

  if (result.errors) {
    throw result.errors
  }

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

  if (node.internal.type === `MarkdownRemark`) {
    const value = createFilePath({ node, getNode })
    createNodeField({
      node,
      name: `date`,
      value: value.match(/\d\d\d\d-\d\d-\d\d/)[0],
    })
    createNodeField({
      node,
      name: `slug`,
      value: `/blog${value}`,
    })
  }
}

exports.createSchemaCustomization = ({ actions }) => {
  const { createTypes, createFieldExtension } = actions
  const typeDefs = `
  type  ProfilesYamlExperience   {
     employer: String
     role: String
     years: String
     location: String
     description: [String]
  }

  type  ProfilesYamlEducation  {
    qualification: String
    name: String
    institution: String
    years: String
    description: [String]

  }

  type ProfilesYaml implements Node @dontInfer {
    slug: String!
    name: String!
    pronouns: String @optional
    github: String
		bio: String
		skills: [String]
    speaks: [String]
    publications: [String]
    experience: [ProfilesYamlExperience]
    education: [ProfilesYamlEducation]
  }
  `

  createFieldExtension({
    name: `optional`,
    extend: () => ({
      resolve(source, args, context, info) {
        if (source[info.fieldName] == null) {
          return ``
        }
        return source[info.fieldName]
      },
    }),
  })

  createTypes(typeDefs)
}

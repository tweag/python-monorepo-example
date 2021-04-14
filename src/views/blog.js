/** @jsx jsx */
import { Fragment } from "react"
import { jsx } from "theme-ui"
import { Grid, Box, Flex } from "theme-ui"

import { useAllTags } from "../hooks"

import { DefaulLayout as Layout } from "../layouts"
import { SEO, Tags, SectionHeading, BlogCard } from "../components"

import pattern1 from "../images/post_pattern1.png"
import pattern2 from "../images/post_pattern2.png"
import pattern3 from "../images/post_pattern3.png"

import homePost from "../images/home_post.png"

const BlogIndex = ({ data }) => {
  const allTags = useAllTags({ withLinks: true, appendGeneral: `all` })

  const [topPost, ...posts] = data.allMarkdownRemark.edges
  const patterns = [pattern1, pattern2, pattern3]
  return (
    <Layout>
      <SEO title="Engineering Blog" />
      <div
        className="section s_white viewport-section transition-section"
        sx={{
          pt: [`65px`, `65px`, `130px`, `130px`, `130px`, `130px`, `160px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed-0"
          gap={`20px`}
          sx={{
            px: [`15px`, `15px`, 0],
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            width: [`100%`, `100%`, `50%`],
          }}
        >
          <SectionHeading
            customSx={{
              justifySelf: `start`,
            }}
          >
            Blog
          </SectionHeading>
          <Tags tags={allTags} />
        </Grid>
        <Flex
          sx={{
            mt: [`20px`, `20px`, `60px`],
            flexWrap: `wrap`,
            justifyContent: `space-between`,
          }}
        >
          <Box
            className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed-0"
            sx={{
              pr: [`15px`, `15px`, 0],
              pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
              mb: [`20px`, `20px`, 0],
              flexBasis: [
                `100%`,
                `100%`,
                `48%`,
                `48%`,
                `48%`,
                `48%`,
                `50%`,
                `47%`,
              ],
            }}
          >
            <BlogCard.TopCard node={topPost.node} />
          </Box>
          <Box
            className="transition-section__transition--slide-fade-in right-in only-above-1 delayed-3"
            sx={{
              flexBasis: [
                `100%`,
                `100%`,
                `45%`,
                `45%`,
                `45%`,
                `50%`,
                `50%`,
                `47%`,
              ],
              pl: [0, 0, `60px`, `60px`, `60px`, `60px`, `60px`],
              pr: [0, 0, `60px`, `60px`, `60px`, `60px`, `120px`],
            }}
          >
            <img sx={{ width: `100%` }} src={homePost} />
          </Box>
        </Flex>
        <Grid
          sx={{
            mt: [`80px`],
            mb: [`40px`],
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            // pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            rowGap: [`60px`, `60px`, `40px`],
            columnGap: [0, 0, `30px`, `50px`, `5%`],
          }}
          columns={[1, 1, 3]}
          // gap={[`60px`, `60px`, 0]}
        >
          {posts.map((post, i) => {
            // Display a pattern periodically.
            const period = 6
            const start = 3
            const pattern = patterns[((i + start) / period) % patterns.length]
            if ((i + start) % period === 0) {
              return (
                <Fragment key={i}>
                  <Box>
                    <img src={pattern} alt="" />
                  </Box>
                  <BlogCard node={post.node} />
                </Fragment>
              )
            } else {
              return <BlogCard key={i} node={post.node} />
            }
          })}
        </Grid>
      </div>
    </Layout>
  )
}

export default BlogIndex

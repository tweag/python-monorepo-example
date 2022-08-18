/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link } from "gatsby"
import { Grid, Text, Box, Flex } from "theme-ui"

import img10 from "../../../images/img10.svg"

function TemplateExamples({ title, examples }) {
  return (
    <Grid
      className="viewport-section transition-section"
      columns={[1, 2]}
      sx={{
        py: [`60px`, `100px`, `150px`],
      }}
    >
      <Grid
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
        columns={1}
        gap={[`50px`]}
        sx={{
          px: [`15px`, `0px`],
          pl: [`15px`, `120px`],
        }}
      >
        <Grid columns={1} gap={[`20px`]}>
          <Text
            as="div"
            sx={{
              fontSize: [
                `18px`,
                `27px`,
                `34px`,
              ],
              lineHeight: [1.1],
              fontWeight: [700],
              mb: [0, `30px`],
            }}
          >
            Examples of Tweag&rsquo;s work in {title.toLowerCase()} includes:
          </Text>
          {examples.map((example, i) => (
            <Flex
              key={i}
              sx={{
                fontSize: [`18px`, `27px`],
              }}
            >
              <div
                sx={{
                  lineHeight: [1],
                }}
                className="icon-arrow-right1"
              />
              {` `}
              <Text
                as="div"
                sx={{
                  pl: [`20px`],
                  lineHeight: [`22px`, 1.1],
                }}
              >
                {example}
              </Text>
            </Flex>
          ))}
        </Grid>
        <Link
          sx={{
            justifySelf: [`start`],
          }}
          to="/contact"
          className="button button-secondary pre-arrow-right button-medium min-5__button-large"
        >
          Contact us
        </Link>
      </Grid>
      <Box
        className="transition-section__transition--slide-fade-in right-in only-above-1 delayed"
        sx={{
          width: [`190px`, `50%`],
          justifySelf: `end`,
          alignSelf: `center`,
        }}
      >
        <img src={img10} alt="" />
      </Box>
    </Grid>
  )
}

export default TemplateExamples

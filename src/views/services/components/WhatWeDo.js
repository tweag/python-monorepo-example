/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Box, Text } from "theme-ui"

import img8 from "../../../images/img8.svg"

import { SectionHeading } from "../../../components"

const content = [
  {
    heading: `Software Engineering`,
    p: `We use tools and programming languages that represent the future
              of reliable software development. Reliability is baked into how we
              build your software.`,
  },
  {
    heading: `DevOps & Infrastructure`,
    p: `Software gets rebuilt and deployed over and over again. We
              specialize in making that process efficient for your business and
              auditable end-to-end so you always know exactly what is in
              production. It's one of our favorite things to do.`,
  },
  {
    heading: `Statistics & Machine Learning`,
    p: `We know how to train and deploy models. We are also here to
              address the growing pains that organizations face in building out
              this new capability.`,
  },
  {
    heading: `Applied Research`,
    p: `There’s nothing more practical than applying the right theory. We
              have a track record in taking the best ideas from research and
              successfully bringing them to market.`,
  },
]

export default function WhatWeDo() {
  return (
    <Grid
      className="section s_white transition-section viewport-section"
      columns={[1, 2]}
      sx={{
        pt: [`60px`],
      }}
    >
      <Grid
        sx={{
          pr: [`15px`, `0px`],
          pl: [`15px`, `50px`, `120px`],
        }}
        gap={[`25px`]}
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
      >
        <SectionHeading
          customSx={{
            justifySelf: `start`,
            alignSelf: `start`,
          }}
        >
          What we do
        </SectionHeading>
        <Grid gap={[`30px`]}>
          {content.map(({ heading, p }) => (
            <Grid key={heading} gap={[`20px`]}>
              <Text
                as="div"
                sx={{
                  fontWeight: [700],
                  fontSize: [`18px`, `34px`],
                  lineHeight: [`22px`, 1.1],
                }}
              >
                <i className="icon-arrow-right1"></i> {heading}
              </Text>
              <Text
                as="div"
                sx={{
                  fontWeight: [400],
                  fontSize: [`18px`, `24px`],
                  lineHeight: [`22px`, 1.1],
                  mx: [`6px`],
                }}
              >
                {p}
              </Text>
            </Grid>
          ))}
        </Grid>
        <Grid></Grid>
      </Grid>
      <Box
        className="transition-section__transition--slide-fade-in right-in only-above-1 delayed"
        sx={{
          width: [`200px`, `100%`],
          justifySelf: `end`,
          alignSelf: `center`,
          display: `flex`,
          justifyContent: `flex-end`,
        }}
      >
        <img
          sx={{
            width: [`100%`, `75%`],
          }}
          src={img8}
          alt=""
        />
      </Box>
    </Grid>
  )
}

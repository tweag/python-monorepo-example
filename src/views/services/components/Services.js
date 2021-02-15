/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Box, Text } from "theme-ui"

import img6 from "../../../images/img6.svg"

import { SectionHeading } from "../../../components"

const content = {
  title: `Services`,
  heading: `Tweag translates ambitious visions into technology solutions that work`,
  paras: [
    `We combine solid engineering principles with new ideas from academia
            to solve complex problems and build critical systems.`,
    `We take on your project as our own. Our engineers embed into your
            team, injecting experience and technical expertise from
            proof-of-concept to product delivery. Achieve your vision with
            confidence by partnering with Tweag.`,
  ],
}

export default function Services() {
  return (
    <Grid
      className="section s_white  viewport-section transition-section"
      columns={[1, 1, 4, 4, 4, 4, 4, 5]}
      gap={[`15px`, `15px`, `30px`]}
      sx={{
        pt: [`60px`, `60px`, `130px`],
      }}
    >
      <Grid
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
        gap={[`35px`, `35px`]}
        sx={{
          px: [`15px`, `15px`, `0px`],
          pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          gridColumnStart: [`auto`, `auto`, 1],
          gridColumnEnd: [`auto`, `auto`, 4, 4, 4, 4, 3, 4],
          gridAutoRows: [`max-content`],
        }}
      >
        <SectionHeading
          customSx={{
            justifySelf: `start`,
            alignSelf: `start`,
          }}
        >
          {content.title}
        </SectionHeading>
        <Text
          sx={{
            fontSize: [`34px`, `34px`, `66px`],
            fontWeight: [700],
            lineHeight: [1],
            textTransform: `uppercase`,
          }}
        >
          {content.heading}
        </Text>
        <Grid
          columns={1}
          gap={[`15px`]}
          sx={{
            fontSize: [`18px`, `18px`, `27px`],
            lineHeight: [1.2, 1.2, `35px`],
            maxWidth: [`100%`, `100%`, `90%`],
          }}
        >
          {content.paras.map((para, idx) => (
            <Text key={idx}>{para}</Text>
          ))}
        </Grid>
      </Grid>
      <Box
        className="transition-section__transition--slide-fade-in right-in only-above-1 delayed"
        sx={{
          alignSelf: `center`,
          gridColumnStart: [`auto`, `auto`, 4, 4, 4, 4, 4, 5],
          gridColumnEnd: [`auto`, `auto`, 6],
          marginTop: `200px`,
          display: [`none`, `none`, `block`],
        }}
      >
        <img src={img6} alt="" />
      </Box>
    </Grid>
  )
}

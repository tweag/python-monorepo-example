/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"
import { Grid, Box, Text } from "theme-ui"

import img7 from "../../../images/img7.svg"

import { Divider } from "../../../components"

const content = {
  main: {
    quotation: (
      <Fragment>
        It&rsquo;s really, really great working with Tweag. The quality of their
        work is top-notch. Their productivity is higher than average. And
        they&rsquo;re domain experts in programming languages.
      </Fragment>
    ),
    from: `Fred Cogny, CTO, NovaDiscovery`,
  },
}

export default function Quote() {
  return (
    <Grid
      className="section s_white transition-section viewport-section"
      columns={[1, 1, 4]}
      gap={[`30px`, `30px`]}
      sx={{
        pt: [`60px`],
      }}
    >
      <Box
        className="transition-section__transition--slide-fade-in left-in only-above-1"
        sx={{
          width: [`200px`, `200px`, `100%`],
          alignSelf: [`center`, `center`, `center`],
        }}
      >
        <img
          sx={{
            width: `100%`,
          }}
          src={img7}
          alt=""
        />
      </Box>
      <Grid
        className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed"
        sx={{
          pr: [`15px`, `15px`, `0px`],
          pl: [`15px`, `15px`, `0px`],
          marginLeft: [`auto`, `auto`, `auto`, `auto`, `auto`, `auto`, `100px`],
          marginRight: [`auto`, `auto`, `auto`],
          my: [`0px`, `0px`, `0px`, `auto`],
          maxWidth: [`100%`, `100%`, `75%`, `75%`, `75%`, `75%`, `50%`],
          gridColumnStart: [`auto`, `auto`, 2],
          gridColumnEnd: [`auto`, `auto`, 5],
          gridAutoRows: [`min-content`],
        }}
        gap={[`20px`]}
      >
        <Text
          as="div"
          sx={{
            fontSize: [`34px`, `34px`],
            lineHeight: [1.1, 1.1],
            fontWeight: [700, 700],
          }}
        >
          {content.main.quotation}
        </Text>
        <Divider
          level={1}
          customSx={{
            width: `100px`,
            pt: `20px`,
          }}
        />
        <Text
          as="div"
          sx={{
            fontSize: [`18px`, `18px`],
            lineHeight: [1.3, 1.3],
          }}
        >
          <i className="icon-arrow-right1"></i> {content.main.from}
        </Text>
      </Grid>
    </Grid>
  )
}

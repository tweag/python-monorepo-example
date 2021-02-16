/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"
import { Grid, Box, Text } from "theme-ui"

import img12 from "../../../images/img12.svg"
import img13 from "../../../images/img13.svg"
import img14 from "../../../images/img14.svg"

import { SectionHeading } from "../../../components"

const content = {
  heading: `Committed to Open Source and advancing knowledge`,
  firstParas: [
    <Fragment key={0}>
      Open source is how we roll. 60% of our team are from academia, where open
      source is the norm. Open source is becoming the norm for companies that
      want to attract the highest levels of engineering talent.
    </Fragment>,
    <Fragment key={1}>
      Why?{` `}
      <strong>
        Open source attracts minds from across the world and helps our customers
        build systems better and faster.
      </strong>
      {` `}
      Code sharing works.
    </Fragment>,
  ],
  secondParas: [
    <Fragment key={0}>
      We employ researchers with strong academic backgrounds and ask them to
      keep pushing the envelope. Their sole focus is to identify future
      directions for programming languages, and we bring research to production
      faster than ever before. Tweagers are working on linear types, dependent
      types and mobile code that runs{` `}
      <strong>everywhere.</strong>
    </Fragment>,
  ],
  thirdParas: [
    <Fragment key={0}>
      <strong>
        Our clients get easy access to some of the best minds in computer
        science.
      </strong>
      {` `}
      They also get Tweagers standing on the shoulders of giants who are right
      behind them. Knowledge sharing works.
    </Fragment>,
    <Fragment key={1}>
      These commitments have helped us build strong, deep, and wide connections
      in open source projects that are changing the tech landscape. Below are
      just a few.
    </Fragment>,
  ],
}

export default function Manifesto() {
  return (
    <div className="section s_white">
      <Grid
        className="viewport-section transition-section"
        columns={[1, 1, 4, 4, 4, 4, 4, 4]}
        gap={[`15px`, `15px`, `30px`]}
        sx={{
          pt: [`60px`, `60px`, `130px`, `130px`, `130px`, `130px`, `160px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1"
          gap={[`35px`, `35px`]}
          sx={{
            px: [`15px`, `15px`, `0px`],
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            gridColumnStart: [`auto`, `auto`, 1],
            gridColumnEnd: [`auto`, `auto`, 4],
            gridAutoRows: [`max-content`],
            width: [`100%`, `100%`, `100%`, `100%`, `100%`, `100%`, `80%`],
          }}
        >
          <SectionHeading
            customSx={{
              justifySelf: `start`,
              alignSelf: `start`,
            }}
          >
            Open source
          </SectionHeading>
          <Text
            sx={{
              fontSize: [`34px`, `34px`, `66px`],
              fontWeight: [700],
              lineHeight: [1],
              textTransform: `uppercase`,
              minHeight: [
                `100px`,
                `100px`,
                `100px`,
                `100px`,
                `100px`,
                `100px`,
                `auto`,
              ],
              mt: [`5px`, `5px`, `15px`],
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
              maxWidth: [`100%`, `100%`, `90%`, `90%`, `90%`, `90%`, `100%`],
            }}
          >
            {content.firstParas.map((para, idx) => (
              <Text key={idx}>{para}</Text>
            ))}
          </Grid>
        </Grid>
        <Box
          className="transition-section__transition--slide-fade-in right-in only-above-1 delayed"
          sx={{
            alignSelf: `start`,
            gridColumnStart: [`auto`, `auto`],
            gridColumnEnd: [`auto`, `auto`],
            marginTop: [`0px`, `0px`, `200px`, `200px`, `200px`],
            marginLeft: [`auto`],
            width: [`100%`, `100%`, `100%`],
            display: [`none`, `none`, `flex`],
            justifyContent: `flex-end`,
          }}
        >
          <img
            sx={{ width: [`100%`, `100%`, `87%`, `87%`, `87%`, `87%`, `80%`] }}
            src={img12}
            alt=""
          />
        </Box>
      </Grid>
      <Grid
        className="section s_white transition-section viewport-section"
        columns={[1, 1, 4]}
        gap={[`30px`, `30px`]}
        sx={{
          pt: [`35px`, `35px`, `60px`, `60px`, 0],
        }}
      >
        <Box
          className="transition-section__transition--slide-fade-in left-in only-above-1"
          sx={{
            width: [`250px`, `250px`, `100%`],
            alignSelf: [`center`, `center`, `center`],
            gridRow: [2, 2, `auto`],
            mt: [0, 0, `50px`],
          }}
        >
          <img
            sx={{
              width: `100%`,
            }}
            src={img13}
            alt=""
          />
        </Box>
        <Grid
          className="transition-section__transition--slide-fade-in right-in only-above-1"
          sx={{
            pr: [`15px`, `15px`, `0px`],
            marginLeft: [
              `auto`,
              `auto`,
              `auto`,
              `auto`,
              `auto`,
              `auto`,
              `auto`,
            ],
            marginRight: [`auto`, `auto`, `auto`],
            mb: [`0px`, `0px`, `0px`, `auto`],
            mt: [0, 0, `30px`, `30px`, `30px`, `30px`, `80px`],
            pl: [`15px`, `15px`, 0, 0, 0, 0, `120px`],
            maxWidth: [`100%`, `100%`, `75%`, `75%`, `75%`, `75%`, `66.7%`],
            gridColumnStart: [`auto`, `auto`, 2],
            gridColumnEnd: [`auto`, `auto`, 5],
            gridAutoRows: [`min-content`],
          }}
          gap={[`20px`]}
        >
          <Grid
            columns={1}
            gap={[`15px`]}
            sx={{
              fontSize: [`18px`, `18px`, `27px`],
              lineHeight: [1.2, 1.2, `35px`],
              maxWidth: [`100%`, `100%`, `90%`, `90%`, `90%`, `90%`, `100%`],
            }}
          >
            {content.secondParas.map((para, idx) => (
              <Text key={idx}>{para}</Text>
            ))}
          </Grid>
        </Grid>
      </Grid>
      <Grid
        className="viewport-section transition-section"
        columns={[1, 1, 4, 4, 4, 4, 4, 4]}
        gap={[`15px`, `15px`, `30px`]}
        sx={{
          pt: [`60px`, `60px`, `45px`, `80px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1"
          gap={[`35px`, `35px`]}
          sx={{
            px: [`15px`, `15px`, `0px`],
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            gridColumnStart: [`auto`, `auto`, 1],
            gridColumnEnd: [`auto`, `auto`, 4],
            gridAutoRows: [`max-content`],
            width: [`100%`, `100%`, `100%`, `100%`, `100%`, `100%`, `80%`],
            mt: [0, 0, 0, 0, 0, `80px`],
          }}
        >
          <Grid
            columns={1}
            gap={[`15px`]}
            sx={{
              fontSize: [`18px`, `18px`, `27px`],
              lineHeight: [1.2, 1.2, `35px`],
              maxWidth: [`100%`, `100%`, `100%`, `100%`, `95%`, `70%`, `85%`],
            }}
          >
            {content.thirdParas.map((para, idx) => (
              <Text key={idx}>{para}</Text>
            ))}
          </Grid>
        </Grid>
        <Box
          className="transition-section__transition--slide-fade-in right-in only-above-1"
          sx={{
            alignSelf: `center`,
            gridColumnStart: [`auto`, `auto`],
            gridColumnEnd: [`auto`, `auto`],
            display: [`none`, `none`, `block`],
            my: [0, 0, `80px`],
          }}
        >
          <img sx={{ width: `100%` }} src={img14} alt="" />
        </Box>
      </Grid>
    </div>
  )
}

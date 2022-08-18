/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"
import { Grid, Text, Box } from "theme-ui"

import { SectionHeading } from "../../../components"

import img9 from "../../../images/img9.svg"

function TemplateFirstSection({ title, headline, paras }) {
  const [firstPara, ...restParas] = paras
  return (
    <Fragment>
      <Grid
        className="viewport-section transition-section"
        columns={[1, 4, 5]}
        gap={[`15px`, `30px`]}
        sx={{
          pt: [`60px`, `130px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1"
          gap={[`25px`, `35px`]}
          sx={{
            px: [`15px`, `0px`],
            pl: [`15px`, `60px`, `120px`],
            gridColumnStart: [`auto`, 1],
            gridColumnEnd: [`auto`, 4, 6],
            gridAutoRows: [`max-content`],
          }}
        >
          <SectionHeading
            customSx={{
              justifySelf: `start`,
              alignSelf: `start`,
            }}
          >
            Key industry
          </SectionHeading>
          <Text
            as="div"
            sx={{
              fontSize: [`18px`, `27px`],
              lineHeight: [1.1],
              fontWeight: [700],
              textTransform: `uppercase`,
            }}
          >
            <i className="icon-arrow-right1"></i> {title}
          </Text>
          <Text
            as="div"
            sx={{
              fontSize: [`34px`, `66px`],
              fontWeight: [700],
              lineHeight: [1],
              textTransform: `uppercase`,
              maxWidth: [`100%`, `100%`, `90%`],
            }}
          >
            {headline}
          </Text>
          <Grid
            columns={1}
            gap={[`15px`]}
            sx={{
              fontSize: [`18px`, `27px`],
              lineHeight: [1.2, `35px`],
              maxWidth: [`100%`, `90%`, `800px`],
              marginTop: [`10px`],
            }}
          >
            <Text as="div">{firstPara}</Text>
          </Grid>
        </Grid>
        {/*  We can use this box for the left layout image like in the services page. */}
        {/* <Box /> */}
      </Grid>
      <Grid
        className="viewport-section transition-section"
        columns={[1, 4]}
        gap={[`35px`]}
        sx={{
          pt: [`35px`, `60px`],
        }}
      >
        <Box
          className="transition-section__transition--slide-fade-in left-in only-above-1"
          sx={{
            width: [`250px`, `100%`],
            alignSelf: [`start`],
            gridRow: [2, 1],
          }}
        >
          <img
            sx={{
              width: `100%`,
            }}
            src={img9}
            alt=""
          />
        </Box>
        <Grid
          className="transition-section__transition--slide-fade-in right-in only-above-1"
          sx={{
            pr: [`15px`, `0px`],
            pl: [`15px`, `0px`],
            marginLeft: [`auto`, `auto`, `100px`],
            marginRight: [`auto`],
            my: [`0px`, `0px`, `auto`],
            maxWidth: [`100%`, `75%`, `50%`],
            gridColumnStart: [`auto`, 2],
            gridColumnEnd: [`auto`, 5],
            gridAutoRows: [`min-content`],
          }}
          gap={[`20px`]}
        >
          {restParas.map((para, i) => (
            <Text
              as="div"
              key={i}
              sx={{
                fontSize: [`18px`, `27px`],
                lineHeight: [1.2, `35px`],
              }}
            >
              {para}
            </Text>
          ))}
        </Grid>
      </Grid>
    </Fragment>
  )
}

export default TemplateFirstSection

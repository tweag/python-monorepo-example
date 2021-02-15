/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Text, Box } from "theme-ui"

import img11 from "../../../images/img11.svg"

import { SectionHeading, Divider } from "../../../components"

function TemplateCaseStudies({ caseStudies }) {
  const { projects, testimonals } = caseStudies
  return (
    <Grid
      className="viewport-section transition-section"
      columns={[1, 1, 3]}
      gap={[`60px`, `60px`, `60px`]}
      sx={{
        py: [`60px`, `60px`, `120px`],
      }}
    >
      <Box
        className="transition-section__transition--slide-fade-in left-in only-above-1 delayed"
        sx={{
          gridRow: [2, 2, 1],
          width: [`190px`, `190px`, `100%`],
          justifySelf: `start`,
          alignSelf: `center`,
        }}
      >
        <img sx={{ width: `100%` }} src={img11} alt="" />
      </Box>
      <Grid
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
        sx={{
          pl: [`15px`, `15px`, `20px`, `20px`, `20px`, `20px`],
          pr: [`15px`, `15px`, `20px`, `20px`, `20px`, `120px`],
          gridColumnStart: [`auto`, `auto`, 2],
          gridColumnEnd: [`auto`, `auto`, 4],
          maxWidth: [`100%`, `100%`, `100%`, `90%`, `90%`, `80%`],
          ml: [0, 0, `auto`],
          my: [0, 0, `auto`],
          gridAutoRows: `max-content`,
        }}
        gap={[`25px`, `25px`]}
      >
        <SectionHeading
          customSx={{
            justifySelf: `start`,
          }}
        >
          Case studies
        </SectionHeading>
        <Text
          sx={{
            fontSize: [`34px`, `34px`, `66px`],
            fontWeight: [700],
            lineHeight: [1],
            textTransform: `uppercase`,
            width: [`80%`, `80%`, `80%`, `80%`, `80%`, `80%`, `90%`],
          }}
        >
          Learn more about our projects
        </Text>
        <Grid
          columns={[2]}
          sx={{
            pt: [`30px`, `30px`],
            pb: [`80px`, `80px`],
          }}
        >
          {projects.map(({ client, description }, i) => (
            <Grid key={i}>
              <Text
                sx={{
                  fontSize: [
                    `18px`,
                    `18px`,
                    `27px`,
                    `27px`,
                    `27px`,
                    `27px`,
                    `34px`,
                  ],
                  fontWeight: [700],
                }}
              >
                {client}
              </Text>
              <Text sx={{ fontSize: [`18px`, `18px`] }}>{description}</Text>
            </Grid>
          ))}
        </Grid>
        <Grid gap={[`20px`]}>
          <Text
            sx={{
              fontSize: [
                `18px`,
                `18px`,
                `27px`,
                `27px`,
                `27px`,
                `27px`,
                `34px`,
              ],
              fontWeight: 700,
              lineHeight: 1.1,
            }}
          >
            {testimonals.main.testimonal}
          </Text>
          <Divider
            level={1}
            customSx={{
              width: `100px`,
              pt: [`20px`],
            }}
          />
          <Text
            sx={{
              fontSize: [`18px`],
            }}
          >
            <i className="icon-arrow-right1" />
            {`  `}
            {testimonals.main.from}
          </Text>
        </Grid>
      </Grid>
    </Grid>
  )
}

export default TemplateCaseStudies

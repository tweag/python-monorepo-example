/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"
import { Grid, Box, Text } from "theme-ui"
import { Link } from "gatsby"

import biotech from "../../images/img3.gif"
import fintech from "../../images/img4.gif"
import vehicles from "../../images/img5.gif"

const industries = [
  {
    h: <Fragment>Biotech</Fragment>,
    p: (
      <Fragment>
        Build statistical models, iterate on them quickly and increase
        productivity.
      </Fragment>
    ),
    src: biotech,
    link: `/industry/biotech`,
  },
  {
    h: <Fragment>Fintech</Fragment>,
    p: (
      <Fragment>
        Minimize risk with high-assurance software, from blockchain to trading
        systems.
      </Fragment>
    ),
    src: fintech,
    link: `/industry/fintech`,
  },
  {
    h: <Fragment>Autonomous vehicles</Fragment>,
    p: (
      <Fragment>
        Put safety first with robust and reliable software, backed by static
        analysis.
      </Fragment>
    ),
    src: vehicles,
    link: `/industry/autonomous`,
  },
]

function Section3() {
  return (
    <Grid
      columns={[1]}
      sx={{
        px: [`15px`, `100px`],
        py: [`40px`],
        textAlign: [`start`, `center`],
      }}
      gap={[`40px`]}
    >
      <Box className="transition--slide-fade-in bottom-in  only-above-1">
        <Text
          as="div"
          sx={{
            fontSize: [`24px`, `34px`, `42px`],
            lineHeight: [1],
            fontWeight: [700],
            textTransform: `uppercase`,
          }}
        >
          KEY INDUSTRIES WE SERVE
        </Text>
      </Box>
      <Grid gap={[`40px`]} columns={[1, 3]}>
        {industries.map(({ h, link, p, src }, i) => (
          <Grid
            key={i}
            sx={{
              transitionDelay: `${0.5 + 0.1 * i}s`,
            }}
            className="transition--slide-fade-in bottom-in  only-above-1"
          >
            <Box>
              <img
                sx={{
                  width: [`50%`, `100%`, `50%`],
                }}
                src={src}
                alt=""
              />
            </Box>
            <Text
              as="div"
              sx={{
                fontSize: [`18px`, `27px`, `34px`],
                lineHeight: [1.1],
                fontWeight: [700],
              }}
            >
              {h}
            </Text>
            <Text
              as="p"
              sx={{
                fontSize: [`18px`],
                lineHeight: [`26px`],
              }}
            >
              {p}
            </Text>
            <Link
              sx={{
                justifySelf: [`start`, `center`],
                alignSelf: `center`,
              }}
              to={link}
              className="button button-secondary pre-arrow-right button-medium min-5__button-large"
            >
              Learn more
            </Link>
          </Grid>
        ))}
      </Grid>
    </Grid>
  )
}

export default Section3

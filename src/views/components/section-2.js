/** @jsx jsx */
import { useEffect, useState } from "react"
import { jsx, Grid, Box, Text } from "theme-ui"
import { Link } from "gatsby"
import visionaries from "../../images/img2.svg"
import visionariesPng from "../../images/img2.png"

function Section2() {
  // check for the Samsung Browser, because
  // its default Dark Mode messes up svgs
  // for reference:
  // https://www.ctrl.blog/entry/samsung-internet-night-mode.html
  // https://github.com/mdn/browser-compat-data/issues/17408
  const [isSamsungBrowser, setIsSamsungBrowser] = useState(false)

  useEffect(() => {
    if (navigator.userAgent.includes(`SamsungBrowser`))
      setIsSamsungBrowser(true)
  }, [])
  return (
    <Grid
      columns={[1, 2]}
      sx={{
        margin: [`auto`, `0`],
        textAlign: [`start`],
        maxWidth: [`100%`],
        px: [`15px`, `60px`, `120px`],
        pt: [`0px`, `90px`],
        pb: [`0px`],
        mt: [`30px`],
        mb: [`40px`],
      }}
      gap={[`50px`, `20%`, `5%`]}
    >
      <Grid
        gap={[`40px`]}
        sx={{
          height: [`auto`, `60%`],
          gridAutoRows: [`auto`, `max-content`],
        }}
        className="transition--slide-fade-in bottom-in  only-above-1"
      >
        <h2
          sx={{
            gridRow: [2, 1],
            maxWidth: [`100%`, `100%`, `400px`],
          }}
        >
          PARTNERING WITH VISIONARIES TO ADVANCE TECHNOLOGY
        </h2>

        <Box>
          <img
            sx={{
              maxWidth: [`100%`, `100%`, `80%`],
              width: [`auto`, `auto`, `70%`],
            }}
            src={isSamsungBrowser ? visionariesPng : visionaries}
            alt=""
          />
        </Box>
      </Grid>
      <Box className="transition--slide-fade-in bottom-in delayed  only-above-1">
        <Grid
          gap={[`40px`]}
          sx={{
            mb: [`60px`],
          }}
        >
          {[
            {
              h: `Scale with confidence`,
              p: `Quickly grow your team with vetted, senior engineers. Tweag
                  provides the expertise needed to execute high-risk,
                  high-reward projects.`,
            },
            {
              h: `Increase developer productivity`,
              p: `Boost developer efficiency with best practices that reduce
                  your time to market.`,
            },
            {
              h: `Solve complex problems`,
              p: `Realize your breakthrough vision. Together, we'll iterate
                  quickly and transform your ideas into products that work.`,
            },
          ].map(({ h, p }) => (
            <Grid key={h} gap={[`12px`, `12px`, `18px`]}>
              <Text
                as="h3"
                sx={{
                  fontSize: [`18px`, `27px`, `34px`],
                }}
              >
                {h}
              </Text>
              <Text
                as="p"
                sx={{
                  m: 0,
                  fontSize: [`14px`, `18px`],
                  lineHeight: [`26px`],
                }}
              >
                {p}
              </Text>
            </Grid>
          ))}
        </Grid>
        <Link
          to="/contact"
          className="button button-secondary pre-arrow-right button-medium min-5__button-large"
        >
          Contact us
        </Link>
      </Box>
    </Grid>
  )
}

export default Section2

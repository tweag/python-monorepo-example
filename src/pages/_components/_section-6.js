/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Box, Text } from "theme-ui"
import { Link } from "gatsby"

function Section6() {
  return (
    <Grid
      columns={[1, 1, 2]}
      sx={{
        py: [`40px`, `40px`, `100px`, `60px`],
      }}
      gap={[`50px`, `50px`]}
    >
      <Grid
        columns={[1, 1, 1, 1, 3, 3, 1]}
        sx={{
          gridRow: [2, 2, 1],
          pr: [`15px`, `15px`, `0px`],
          pl: [`15px`, `15px`, `50px`, `50px`, `50px`, `50px`, `100px`],
          gridAutoRows: [`max-content`],
          alignSelf: `center`,
        }}
        gap={[`30px`, `30px`]}
        className="transition--slide-fade-in bottom-in  only-above-1"
      >
        <Box
          sx={{
            gridColumnStart: [`auto`, `auto`, `auto`, `auto`, 1, 1, `auto`],
            gridColumnEnd: [`auto`, `auto`, `auto`, `auto`, 3, 3, `auto`],
          }}
        >
          <Text
            sx={{
              fontSize: [
                `24px`,
                `24px`,
                `34px`,
                `34px`,
                `34px`,
                `34px`,
                `42px`,
              ],
              lineHeight: [1.2, 1.2],
              fontWeight: 700,
              textTransform: `uppercase`,
              maxWidth: `400px`,
            }}
          >
            Ready to achieve your big vision?
          </Text>
        </Box>
        <Box>
          <Link to="/contact" className="btn">
            Contact us
          </Link>
        </Box>
      </Grid>
      <Box
        sx={{
          display: `flex`,
          justifyContent: [
            `center`,
            `center`,
            `center`,
            `center`,
            `center`,
            `flex-end`,
            `flex-end`,
            `flex-end`,
          ],
        }}
        className="transition--slide-fade-in right-in delayed  only-above-1"
      >
        <div
          sx={{
            width: [
              `100%`,
              `100%`,
              `100%`,
              `100%`,
              `715px`,
              `715px`,
              `715px`,
              `715px`,
            ],
          }}
          className="image-holder backdrop2"
        ></div>
      </Box>
    </Grid>
  )
}

export default Section6

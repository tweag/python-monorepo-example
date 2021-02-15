/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Box, Text } from "theme-ui"

function Section4() {
  return (
    <Grid
      columns={[1, 1, 2]}
      gap={[`50px`, `50px`, `50px`, `150px`]}
      sx={{
        my: [`40px`],
      }}
    >
      <Box
        sx={{
          pr: [`15px`, `15px`, `0px`],
          pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `140px`],
          gridRow: [2, 2, 1],
        }}
        className="transition--slide-fade-in bottom-in  only-above-1"
      >
        <Text
          as="p"
          sx={{
            fontSize: [`24px`, `24px`, `27px`, `27px`, `27px`, `27px`, `38px`],
            lineHeight: [
              `30px`,
              `30px`,
              `40px`,
              `40px`,
              `40px`,
              `40px`,
              `46px`,
            ],
            fontWeight: [700, 700],
          }}
        >
          At Tweag, we drive purposeful innovation through lasting software. We
          apply mathematics, computer science and the methods of open source to
          advance software engineering.
        </Text>
      </Box>
      <Box
        sx={{
          display: `flex`,
          justifyContent: `flex-end`,
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
              `100%`,
              `100%`,
              `100%`,
              `715px`,
            ],
          }}
          className="image-holder backdrop"
          style={{ animation: `none` }}
        ></div>
      </Box>
    </Grid>
  )
}

export default Section4

/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Box, Text } from "theme-ui"
import { Link } from "gatsby"

import Backdrop from "./Backdrop"

function CallToActionFooter({
  title,
  backdropVariant = ``,
  transitionClass,
  customWrapperSx,
}) {
  return (
    <Grid
      columns={[1, 1, 2]}
      sx={{
        pt: [`40px`, `40px`, `100px`, `60px`],
        pb: [`40px`, `40px`, `50px`, `60px`],
        ...customWrapperSx,
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
        className={`${transitionClass} bottom-in  only-above-1`}
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
            {title}
          </Text>
        </Box>
        <Box>
          <Link
            to="/contact"
            className="button button-medium min-5__button-large   button-secondary pre-arrow-right"
          >
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
        className={`${transitionClass} right-in delayed  only-above-1`}
      >
        <Backdrop
          variant={backdropVariant}
          customSx={{
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
        />
      </Box>
    </Grid>
  )
}

export default CallToActionFooter

/** @jsx jsx */
// eslint-disable-next-line no-unused-vars
import { jsx, Box, SxStyleProp } from "theme-ui"
import { keyframes } from "@emotion/react"

import friseBiotech from "../images/frise_biotech.svg"
import friseBlogHome from "../images/frise_blog_home.svg"
import friseBlogPost from "../images/frise_blog_post.svg"
import friseHome from "../images/frise_home.svg"
import friseJoinus from "../images/home_joinus_frise.svg"
import friseOpensource from "../images/frise_opensource.svg"
import friseServices from "../images/frise_services.svg"
import friseUseCases from "../images/frise_use_cases.svg"

const scroll = keyframes({ to: { backgroundPosition: `-400px 0px` } })

const imgVariants = {
  1: friseJoinus,
  2: friseHome,
  3: friseServices,
  4: friseBiotech,
  5: friseOpensource,
  6: friseBlogPost,
  7: friseBlogHome,
  8: friseUseCases,
}

/**
 * Renders a backdrop background with animation.
 * @typedef Props
 * @property {number} variant
 * @property {SxStyleProp} customSx
 * - You can pass all the theme-ui sx props to furthur style the divider.
 *
 *  @param {Props} props
 */
function Backdrop({ customSx, variant }) {
  return (
    <Box
      sx={{
        animationName: scroll,
        animationDuration: `10s`,
        animationTimingFunction: `linear`,
        animationIterationCount: `infinite`,
        background: `url(${imgVariants[variant]})`,
        backgroundSize: `400px 200px`,
        minHeight: `200px`,
        ...customSx,
      }}
    />
  )
}

export default Backdrop

/** @jsx jsx */
// eslint-disable-next-line no-unused-vars
import { jsx, SxStyleProp } from "theme-ui"
import { Box } from "theme-ui"

/**
 * Renders a Divider component
 *
 * @typedef Props
 * @property {number | number[]} level
 * - the width of the divider;
 * - default to 1 i.e 1px.
 * - You can pass an array of numbers that will be used by the theme-ui jsx for different break-points;
 * @property {SxStyleProp} customSx
 * - You can pass all the theme-ui sx props to furthur style the divider.
 *
 * @param {Props} props
 */
export default function Divider({ level = 1, customSx }) {
  let l = level
  if (!Array.isArray(level)) l = [level]

  return (
    <Box
      sx={{
        height: l.map(mag => `${mag}px`),
        borderBottom: l.map(mag => `${mag}px solid var(--fg-color)`),
        ...customSx,
      }}
    />
  )
}

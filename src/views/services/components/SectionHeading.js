/* eslint-disable no-unused-vars */
/** @jsx jsx */
import { jsx, SxStyleProp } from "theme-ui"
import { Text } from "theme-ui"

/**
 * Renders a Section Heading component. Text underlined by a strong border.
 *
 * @typedef Props
 * @property {SxStyleProp} customSx
 * - You can pass all the theme-ui sx props to furthur style the header.
 * @param {Props} props
 */
function SectionHeading({ customSx, children }) {
  return (
    <Text
      sx={{
        fontSize: [`18px`, `18px`],
        lineHeight: [`22px`, `22px`],
        fontWeight: [400, 400],
        borderBottom: [`1px solid var(--fg-color)`],
        pb: [`10px`],
        ...customSx,
      }}
    >
      {children}
    </Text>
  )
}

export default SectionHeading

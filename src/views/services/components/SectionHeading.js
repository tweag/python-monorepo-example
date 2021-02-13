/** @jsx jsx */
import { jsx } from "theme-ui"
import { Text } from "theme-ui"

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

import React from "react"
import { ThemeUIStyleObject } from "theme-ui"
import { Text } from "theme-ui"

type Props = {
  customSx?: ThemeUIStyleObject
}

/**
 * Renders a Section Heading component. Text underlined by a strong border.
 *
 * - You can pass all the theme-ui sx props to furthur style the header.
 * @param props
 */
const SectionHeading: React.FC<React.PropsWithChildren<Props>> = ({
  customSx,
  children,
}): JSX.Element => {
  return (
    <Text
      as="div"
      sx={{
        fontSize: [`18px`],
        lineHeight: [`22px`],
        fontWeight: [400],
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

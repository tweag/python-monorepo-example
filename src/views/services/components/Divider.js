/** @jsx jsx */
import { jsx } from "theme-ui"
import { Box } from "theme-ui"

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

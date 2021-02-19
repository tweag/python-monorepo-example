/** @jsx jsx */
// eslint-disable-next-line no-unused-vars
import { jsx, SxStyleProp } from "theme-ui"
import { Box } from "theme-ui"

function getRotation(direction) {
  let rotationMagnitude = 0
  if (direction === `down`) rotationMagnitude = 135
  if (direction === `up`) rotationMagnitude = -45
  if (direction === `left`) rotationMagnitude = 225
  if (direction === `right`) rotationMagnitude = 45
  return rotationMagnitude
}

/**
 * Renders a Arrow component.
 *
 * @typedef Props
 * @property {string} direction
 * - The direction of the arrow.
 * - It can be up | down | right | left.
 * @property {number | number[]} level
 * - the width of the arrow;
 * - default to 1 i.e 1px.
 * - You can pass an array of numbers that will be used by the theme-ui jsx for different break-points.
 * @property {string} size
 * - The size of the arrow i.e the width and height of the arrow.
 * @property {string} variant
 * - The variant of the arrow.
 * - It can be solid | dotted | dashed | double | groove | ridge.
 *
 * @property {SxStyleProp} customSx
 * - You can pass all the theme-ui sx props to furthur style the arrow.
 * - Note: If you pass a transform value, then you need to take care of the direction of the arrow yourself.
 * - The values are as follows:
 * - up = -45deg, down = 135deg, left = 225deg, right = 45deg
 * - Pass them in the rotate value of the transform.
 *
 * @property {string} className
 * - This will be passed directly to the className property of the arrow.
 * - Can be used to apply any class to the arrow.
 * @property {string} color
 * - Color of the arrow.
 *
 * @param {Props} props
 */
const Arrow = ({
  level = 1,
  color = `black`,
  size = `40px`,
  variant = `solid`,
  direction = `down`,
  customSx = {},
  className = ``,
}) => {
  const l = Array.isArray(level) ? level : [level]
  const s = Array.isArray(size) ? size : [size]
  const d = Array.isArray(direction) ? direction : [direction]

  return (
    <Box
      className={className}
      sx={{
        borderRight: l.map(
          borderWidth => `${borderWidth}px ${variant} ${color}`
        ),
        borderTop: l.map(borderWidth => `${borderWidth}px ${variant} ${color}`),
        transform: d.map(dir => `rotate(${getRotation(dir)}deg)`),
        width: s,
        height: s,
        ...customSx,
      }}
    />
  )
}

export default Arrow

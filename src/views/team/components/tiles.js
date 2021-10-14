/** @jsx jsx */
import { jsx } from "theme-ui"

import styles from "../styles/tiles.module.css"

const TILE_COLORS = [`#005642`, `#30C179`, `#FBCEB4`, `#FFFFFF`]
const ROUNDINGS = [``, styles.roundLeftCorner, styles.roundRightCorner]

/**
 * @param {{x: number, y: number}} start
 * @param {number} height
 * @param {number} width
 * @returns {{
 *  "--start-column": number,
 *  "--end-column": number,
 *  "--start-row": number,
 *  "--end-row": number
 * } | {}}
 */
function parsePositionalStyles(start, width, height) {
  const result = {}

  if (!!start && !!height && !!width) {
    result[`--start-column`] = start.x
    result[`--end-column`] = start.x + width
    result[`--start-row`] = start.y
    result[`--end-row`] = start.y + height
  }

  return result
}

/**
 * @param {{
 *  colorIndex?: 0 | 1 | 2 | 3,
 *  rounding?: 0 | 1 | 2,
 *  start?: {x: number, y: number},
 *  height?: number,
 *  width?: number,
 *  key?: number | string
 * }} props
 * @returns {JSX.Element}
 */
export const ColorTile = ({
  colorIndex = Math.floor((Math.random() * 4) % 4),
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  key,
}) => {
  const positionalStyles = parsePositionalStyles(start, width, height)

  return (
    <div
      className={[
        styles.tile,
        styles.positionedTile,
        styles.coloredTile,
        ROUNDINGS[rounding],
      ].join(` `)}
      style={{
        "--tile-color": TILE_COLORS[colorIndex],
        ...positionalStyles,
      }}
      key={key}
    />
  )
}

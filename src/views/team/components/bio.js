// eslint-disable-next-line no-unused-vars
import React from "react"

import { parsePositionalStyles } from "../utils/ajustments"
import styles from "../styles/bio.module.css"

const RIGHT_ROUNDINGS = [
  styles.roundTopRightCorner,
  styles.roundBottomRightCorner,
  styles.roundBottomLeftCorner,
]
const LEFT_ROUNDINGS = [
  styles.roundTopLeftCorner,
  styles.roundBottomLeftCorner,
  styles.roundBottomRightCorner,
]

/**
 * @param {{
 *  rounding: 0 | 1 | 2,
 *  person: {
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string
 *  },
 *  start: {x: number, y: number},
 *  height: number,
 *  width: number,
 *  key: number | string,
 *  relativePosition: 'left' | 'right' | 'none'
 * }} props
 * @returns {JSX.Element}
 */
const Bio = ({
  person,
  rounding,
  start,
  height,
  width,
  key,
  relativePosition,
}) => {
  let roundingToUse
  if (relativePosition === `left`) {
    roundingToUse = LEFT_ROUNDINGS[rounding]
  } else if (relativePosition === `right`) {
    roundingToUse = RIGHT_ROUNDINGS[rounding]
  } else {
    roundingToUse = `${LEFT_ROUNDINGS[0]} ${LEFT_ROUNDINGS[2]}`
  }

  const positionalStyles = parsePositionalStyles(start, width, height)
  return (
    <div className={[roundingToUse, ...positionalStyles].join(` `)} key={key} />
  )
}

export default Bio

// eslint-disable-next-line no-unused-vars
import React from "react"

import { parsePositionalStyles } from "../utils/ajustments"
import styles from "../styles/bio.module.css"
import { positionedTile } from "../styles/tiles.module.css"

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

const CloseButton = () => {
  const onClick = event => {
    event.target.dispatchEvent(new Event(`close-bio`, { bubbles: true }))
  }
  return <div className={styles.closeButton} onClick={onClick} />
}

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

  console.log(JSON.stringify({ height, width, start }, null, 2))
  const positionalStyles = parsePositionalStyles(start, width, height)
  console.log(JSON.stringify(positionalStyles, null, 2))
  return (
    <div
      className={[roundingToUse, styles.bio, positionedTile].join(` `)}
      style={{ ...positionalStyles }}
      key={key}
    >
      <div className={styles.header}>
        <div className={styles.personName}>{person.name}</div>
        <div className={styles.personRole}>{person.role}</div>
        <CloseButton />
      </div>
      <div className={styles.tags}>
        {person.tags.map(tag => (
          <div className={styles.tag} key={tag}>
            {tag}
          </div>
        ))}
      </div>
      <div className={styles.shortDescription}></div>
      <div className={styles.longTextContainer}>
        <div className={styles.longTextScrollable}>{person.bio}</div>
      </div>
      <div className={styles.links}></div>
    </div>
  )
}

export default Bio

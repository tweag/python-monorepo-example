// eslint-disable-next-line no-unused-vars
import React, { createContext, useEffect, useRef } from "react"
import { v4 as uuid } from "uuid"

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
    event.target.dispatchEvent(new Event(`toggle-bio`, { bubbles: true }))
  }
  return <div className={styles.closeButton} onClick={onClick} />
}

/**
 * @param {string} longText
 * @return {JSX.Element[]}
 */
function parseBioLongText(longText) {
  const paragraphs = longText.split(`\n\n`)
  return paragraphs.map(paragraph => <p key={uuid()}>{paragraph}</p>)
}

export const BioContext = createContext(null)

/**
 * @param {{
 *  rounding: 0 | 1 | 2,
 *  person: {
 *    name: string
 *    pronouns: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string,
 *    shortDescription: string,
 *    links: {[linkName: string]: string}
 *  },
 *  start: {x: number, y: number},
 *  height: number,
 *  width: number,
 *  key: number | string,
 *  relativePosition: 'left' | 'right' | 'none'
 * }} props
 * @returns {JSX.Element}
 */
const Bio = ({ person, rounding, start, height, width, relativePosition }) => {
  let roundingToUse
  if (relativePosition === `left`) {
    roundingToUse = LEFT_ROUNDINGS[rounding]
  } else if (relativePosition === `right`) {
    roundingToUse = RIGHT_ROUNDINGS[rounding]
  } else {
    roundingToUse = `${LEFT_ROUNDINGS[0]} ${LEFT_ROUNDINGS[2]}`
  }
  const positionalStyles = parsePositionalStyles(start, width, height)

  // Oveflow Detection Stuff
  const longTextRef = useRef()
  useEffect(() => {
    const target = longTextRef.current
    if (target.offsetHeight < target.scrollHeight) {
      const overflowEvent = new Event(`bio-overflow`, { bubbles: true })
      target.dispatchEvent(overflowEvent)
    }
  }, [height, width])

  return (
    <div
      className={[roundingToUse, styles.bio, positionedTile].join(` `)}
      style={{ ...positionalStyles }}
    >
      <div className={styles.header}>
        <div className={styles.personName}>{person.name}</div>
        <div className={styles.personRole}>
          {[person.role, person.pronouns].filter(Boolean).join(`, `)}
        </div>
        <CloseButton />
      </div>
      <div className={styles.tags}>
        {person.tags.map(tag => (
          <div className={styles.tag} key={tag}>
            {tag}
          </div>
        ))}
      </div>
      <div className={styles.shortDescription}>{person.shortDescription}</div>
      <div className={styles.longTextContainer}>
        <div className={styles.longTextScrollable} ref={longTextRef}>
          {parseBioLongText(person.bio)}
        </div>
      </div>
      <div className={styles.links}>
        {Object.entries(person.links).map(([linkName, url]) => (
          <a
            className={styles.link}
            href={url}
            target="_blank"
            rel="noreferrer"
            key={linkName + url}
          >
            {linkName}
          </a>
        ))}
      </div>
    </div>
  )
}

export default Bio

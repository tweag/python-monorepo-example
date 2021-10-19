/** @jsx jsx */
import { jsx } from "theme-ui"
// eslint-disable-next-line no-unused-vars
import { useState, MutableRefObject } from "react"

import { useFilteredMode } from "../hooks/tiles-hooks"
import { parsePositionalStyles } from "../utils/ajustments"
import styles from "../styles/tiles.module.css"

const TILE_COLORS = [`#005642`, `#30C179`, `#FBCEB4`, `#FFFFFF`]
const ROUNDINGS = [``, styles.roundLeftCorner, styles.roundRightCorner]

/**
 * @param {string[]} tags
 * @param {RegExp} filterRegExp
 * @returns {boolean}
 */
function testTags(tags, filterRegExp) {
  let result = false

  for (const tag of tags) {
    result = result || filterRegExp.test(tag)
  }

  return result
}

function generateTagFilterEventIssuer(tag) {
  return event => {
    const eventSource = event.target
    const currentEvent = new Event(`filter`, { bubbles: true })
    currentEvent.filterString = tag
    eventSource.dispatchEvent(currentEvent)
  }
}

/**
 * @param {{
 *  person: {
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string
 *  },
 *  rounding: 0 | 1 | 2,
 *  start: {x: number, y: number},
 *  height: number,
 *  width: number,
 * }} options
 */
function generateShowBioEventIssuer({
  person,
  start,
  height,
  width,
  rounding,
}) {
  return event => {
    const eventToFire = new Event(`bio`, { bubbles: true })
    eventToFire.tileInfo = { person, start, height, width, rounding }
    event.target.dispatchEvent(eventToFire)
  }
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
  colorIndex = Math.floor((Math.random() * 3) % 3),
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
export const BlankTile = ({
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  key,
}) => {
  const colorTileProps = {
    rounding,
    start,
    height,
    width,
    key,
    colorIndex: 3,
  }

  return <ColorTile {...colorTileProps} />
}

/**
 * @param {{
 *  person: {
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string
 *  },
 *  photo: string,
 *  rounding?: 0 | 1 | 2,
 *  start?: {x: number, y: number},
 *  height?: number,
 *  width?: number,
 *  key?: number | string
 * }} props
 */
export const ProfileTile = ({
  person,
  photo,
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  key,
}) => {
  // Invoke bio
  const clickHandler = generateShowBioEventIssuer({
    person,
    rounding,
    start,
    height,
    width,
  })

  // Positional styles
  const positionalStyles = parsePositionalStyles(start, width, height)

  // Auto-filter stuff
  const filter = filterString => {
    const filterRegExp = new RegExp(filterString, `i`)
    const nameCompatible = filterRegExp.test(person.name)
    const roleCompatible = filterRegExp.test(person.role)
    const tagCompatible = testTags(person.tags, filterRegExp)

    return nameCompatible || roleCompatible || tagCompatible
  }

  const show = useFilteredMode(filter)

  return show ? (
    <div
      className={[
        styles.tile,
        styles.positionedTile,
        styles.profileTile,
        ROUNDINGS[rounding],
      ].join(` `)}
      style={{
        ...positionalStyles,
      }}
      key={key}
      onClick={clickHandler}
    >
      <div
        className={styles.profilePhoto}
        style={{
          "--profile-picture": `url(${photo ?? `#`})`,
        }}
      />
      <div className={styles.profileName}>{person.name}</div>
    </div>
  ) : (
    <ColorTile key={key} start={start} width={width} height={height} />
  )
}

/**
 * @param {{
 *  tag: string,
 *  rounding?: 0 | 1 | 2,
 *  start?: {x: number, y: number},
 *  height?: number,
 *  width?: number,
 *  key?: number | string
 * }} props
 */
export const TagTile = ({
  tag,
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  key,
}) => {
  const positionalStyles = parsePositionalStyles(start, width, height)
  const [state, setState] = useState(false)
  const onClick = event => {
    generateTagFilterEventIssuer(!state ? tag : ``)(event)
    setState(!state)
  }
  const filter = filterString => {
    const filterRegExp = new RegExp(filterString, `i`)
    return filterRegExp.test(tag)
  }
  const show = useFilteredMode(filter)

  return show ? (
    <div
      className={[
        styles.tile,
        styles.positionedTile,
        styles.tagTile,
        state ? styles.active : ``,
        ROUNDINGS[rounding],
      ].join(` `)}
      style={{
        ...positionalStyles,
      }}
      key={key}
      onClick={onClick}
    >
      {tag}
    </div>
  ) : (
    <ColorTile key={key} start={start} width={width} height={height} />
  )
}

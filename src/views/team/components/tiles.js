/** @jsx jsx */
import { jsx } from "theme-ui"
// eslint-disable-next-line no-unused-vars
import { useState, MutableRefObject, useRef } from "react"

import { useFilteredMode } from "../hooks/tiles-hooks"
import { parsePositionalStyles } from "../utils/ajustments"
import styles from "../styles/tiles.module.css"

export const TILE_COLORS = [`#005642`, `#30C179`, `#FBCEB4`, `#FFFFFF`]
export const ROUNDINGS = [``, styles.roundLeftCorner, styles.roundRightCorner]

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
 *  colorIndex?: 0 | 1 | 2 | 3,
 *  rounding?: 0 | 1 | 2,
 *  start?: {x: number, y: number},
 *  height?: number,
 *  width?: number,
 *  key?: number | string,
 *  id?: string
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
  id,
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
      id={id}
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
 *  key?: number | string,
 *  id?: string
 * }} props
 * @returns {JSX.Element}
 */
export const BlankTile = ({
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  key,
  id,
}) => {
  const colorTileProps = {
    rounding,
    start,
    height,
    width,
    key,
    colorIndex: 3,
    id,
  }

  return <ColorTile {...colorTileProps} />
}

/**
 * @param {{
 *  tag: string,
 *  rounding?: 0 | 1 | 2,
 *  start?: {x: number, y: number},
 *  height?: number,
 *  width?: number,
 *  key?: number | string,
 *  id?: string
 * }} props
 */
export const TagTile = ({
  tag,
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  key,
  id,
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
      id={id}
    >
      <span className={styles.tagText}>{tag}</span>
    </div>
  ) : (
    <ColorTile key={key} start={start} width={width} height={height} />
  )
}

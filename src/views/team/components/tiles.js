/** @jsx jsx */
import { jsx } from "theme-ui"
// eslint-disable-next-line no-unused-vars
import { useState, MutableRefObject, useRef } from "react"

import { parsePositionalStyles } from "../utils/ajustments"
import { useSearchContext } from "../utils/search"
import styles from "../styles/tiles.module.css"

export const TILE_COLORS = [`#005642`, `#30C179`, `#FBCEB4`, `#FFFFFF`]
export const ROUNDINGS = [``, styles.roundLeftCorner, styles.roundRightCorner]

/**
 * @param {MutableRefObject<HTMLElement>} eventSource
 * @param {string} searchString
 */
function emmitFilterEvent(eventSource, searchString) {
  const source = eventSource.current
  const event = new Event(`filter`, { bubbles: true })
  event.filterString = searchString
  source.dispatchEvent(event)
}

/**
 * @param {{
 *  colorIndex?: 0 | 1 | 2 | 3,
 *  rounding?: 0 | 1 | 2,
 *  start?: {x: number, y: number},
 *  height?: number,
 *  width?: number,
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
 *  id?: string
 * }} props
 * @returns {JSX.Element}
 */
export const BlankTile = ({
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  id,
}) => {
  const colorTileProps = {
    rounding,
    start,
    height,
    width,
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
 *  id?: string
 * }} props
 */
export const TagTile = ({
  tag,
  rounding = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  id,
}) => {
  const positionalStyles = parsePositionalStyles(start, width, height)
  const mainRef = useRef(null)
  const searchManager = useSearchContext()
  const active = searchManager.containsTag(tag)
  const onClick = () => {
    if (active) {
      searchManager.removeTag(tag)
      emmitFilterEvent(mainRef, searchManager.searchString)
    } else {
      searchManager.addTag(tag)
      emmitFilterEvent(mainRef, searchManager.searchString)
    }
  }

  return (
    <div
      className={[
        styles.tile,
        styles.positionedTile,
        styles.tagTile,
        active ? styles.active : ``,
        ROUNDINGS[rounding],
      ].join(` `)}
      style={{
        ...positionalStyles,
      }}
      onClick={onClick}
      id={id}
      ref={mainRef}
    >
      <span className={styles.tagText}>{tag}</span>
    </div>
  )
}

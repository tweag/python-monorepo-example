import React, { useContext, useState } from "react"
import { ROUNDINGS, TILE_COLORS } from "./tiles"
import { parsePositionalStyles } from "../utils/ajustments"
import { BioContext } from "./bio"
import { useSearchContext } from "../utils/search"
import styles from "../styles/tiles.module.css"

/**
 * @param {{
 *  person: {
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string,
 *    shortDescription: string,
 *    links: {[linkName: string]: string},
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
    const eventToFire = new Event(`toggle-bio`, { bubbles: true })
    eventToFire.tileInfo = { person, start, height, width, rounding }
    event.target.dispatchEvent(eventToFire)
  }
}

/**
 * @param {{
 *  person: {
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string,
 *    shortDescription: string,
 *    links: {[linkName: string]: string}
 *  },
 *  photo: string,
 *  rounding?: 0 | 1 | 2,
 *  colorIndex?: 0 | 1 | 2,
 *  start?: {x: number, y: number},
 *  height?: number,
 *  width?: number,
 *  key?: number | string,
 *  id?: string
 *  active: boolean = false
 * }} props
 */
export const ProfileTile = ({
  person,
  photo,
  rounding = Math.floor((Math.random() * 3) % 3),
  colorIndex = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  id,
  active = false,
}) => {
  // Lunr search context
  const searchManager = useSearchContext()

  // Bio Context
  const bioContextValue = useContext(BioContext)

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

  const show = searchManager.activeProfiles.includes(person.slug)

  // Some flags
  const [imageLoaded, setImageLoaded] = useState(false)
  const notShowingTile =
    !show ||
    (!!bioContextValue && bioContextValue !== person.slug) ||
    !imageLoaded

  /**
   * @param {string} imgDataUrl
   * @returns {JSX.Element}
   */
  const result = (
    <div
      className={[
        styles.tile,
        styles.positionedTile,
        styles.profileTile,
        ROUNDINGS[rounding],
        notShowingTile
          ? styles.colorizeTile
          : active
          ? ``
          : styles.decolorizeTile,
        active ? styles.forceActive : ``,
      ].join(` `)}
      style={{
        ...positionalStyles,
        "--tile-color": TILE_COLORS[colorIndex],
      }}
      onClick={notShowingTile ? null : clickHandler}
      onPointerOut={event => {
        const target = event.currentTarget
        const removeForceActive = () =>
          target.classList.remove(styles.forceActive)
        window.addEventListener(`pointermove`, removeForceActive, {
          once: true,
        })
        setTimeout(() => {
          window.removeEventListener(`pointermove`, removeForceActive)
        }, 100)
      }}
      id={id}
    >
      <div className={styles.shadowContainer}>
        <img
          className={styles.profilePhoto}
          onLoad={() => setImageLoaded(true)}
          src={photo}
          alt={person.slug}
          height={500}
          width={500}
          loading="lazy"
        />
      </div>
      <div className={styles.profileName}>{person.name}</div>
    </div>
  )

  return result
}

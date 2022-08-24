/** @jsx jsx */
import { jsx, useThemeUI } from "theme-ui"
import { useContext, useState } from "react"
import { ROUNDINGS, TILE_COLORS } from "./tiles"
import { parsePositionalStyles } from "../utils/ajustments"
import { BioContext } from "./bio"
import { useSearchContext } from "../utils/search"
import * as styles from "../styles/tiles.module.css"

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

  const keyPressHandler = event => {
    if (event.key !== `Enter`) return
    clickHandler(event)
  }

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
  const { theme: t } = useThemeUI()
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
      onKeyPress={notShowingTile ? null : keyPressHandler}
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
      tabIndex={notShowingTile ? `-1` : `0`}
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
      <div
        className="profileName"
        css={`
          position: absolute;
          z-index: 100;
          font-weight: 600;
          font-size: 0.8rem;
          left: 0px;
          bottom: 0px;
          color: white;
          padding: 0.3rem;
          transition: all var(--animations-duration);

          @media screen and (min-width: ${t.breakpoints[1]}) {
            padding: 0.5rem;
            font-size: 1rem;
          }

          @media screen and (min-width: ${t.breakpoints[4]}) {
            padding: 1rem;
            font-weight: bold;
          }

          @media screen and (max-width: ${t.breakpoints[1]}) {
            padding: 0.3rem;
            font-weight: 600;
            font-size: 0.8rem;
          }
        `}
      >
        {person.name}
      </div>
    </div>
  )

  return result
}

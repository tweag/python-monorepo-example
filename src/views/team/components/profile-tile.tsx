import * as React from "react"
import { css, get, useThemeUI } from "theme-ui"
import { useContext, useEffect, useRef, useState } from "react"
import { ROUNDINGS, TILE_COLORS } from "./tiles"
import { parsePositionalStyles } from "../utils/ajustments"
import { BioContext } from "./bio"
import { useSearchContext } from "../utils/search"
import * as styles from "../styles/tiles.module.css"

type Props = {
  person: Person
  photo: string
  rounding?: 0 | 1 | 2
  colorIndex?: 0 | 1 | 2
  start?: { x: number; y: number }
  height?: number
  width?: number
  key?: number | string
  active: boolean
  onClick: (tileInfo: TileInfo) => void
}

export type Person = {
  name: string
  bio: string
  role: string
  tags: string[]
  slug: string
  shortDescription: string
  links: { [linkName: string]: string }
  pronoums: string
}

export type TileInfo = {
  person: Person
  start?: { x: number; y: number }
  height?: number
  width?: number
  rounding: number
}

// export type ToggleBioEvent = CustomEvent<TileInfo>

export const ProfileTile: React.FC<Props> = ({
  person,
  photo,
  rounding = Math.floor((Math.random() * 3) % 3),
  colorIndex = Math.floor((Math.random() * 3) % 3),
  start,
  height,
  width,
  active = false,
  onClick,
}) => {
  // Lunr search context
  const searchManager = useSearchContext()

  const divRef = useRef<HTMLDivElement>(null)

  // Bio Context
  const bioContextValue = useContext(BioContext)
  const isBioOpen = !!bioContextValue
  const isMyBioOpen = bioContextValue === person.slug

  const toggleBio = () =>
    onClick({
      person,
      start,
      height,
      width,
      rounding,
    })

  useEffect(() => {
    if (window.location.hash === `#${person.slug}` && !isMyBioOpen) {
      toggleBio()
    }
  }, [])

  useEffect(() => {
    if (!isMyBioOpen) return

    divRef.current?.scrollIntoView({
      behavior: `smooth`,
      block: `start`,
      inline: `center`,
    })
  }, [isMyBioOpen])

  const keyPressHandler = (event: React.KeyboardEvent) => {
    if (event.key !== `Enter`) return
    toggleBio()
  }

  // Positional styles
  const positionalStyles = parsePositionalStyles(start, width, height)

  const show = searchManager.activeProfiles.includes(person.slug)

  // Some flags
  const [imageLoaded, setImageLoaded] = useState(false)
  const notShowingTile = !show || (isBioOpen && !isMyBioOpen) || !imageLoaded

  const { theme: t } = useThemeUI()

  const result = (
    <div
      ref={divRef}
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
        scrollMarginTop: `8rem`,
      }}
      onClick={notShowingTile ? undefined : toggleBio}
      onKeyPress={notShowingTile ? undefined : keyPressHandler}
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
      id={person.slug}
      tabIndex={notShowingTile ? -1 : 0}
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
        css={css`
          position: absolute;
          z-index: 100;
          font-weight: 600;
          font-size: 0.8rem;
          left: 0px;
          bottom: 0px;
          color: white;
          padding: 0.3rem;
          transition: all var(--animations-duration);

          @media screen and (min-width: ${get(t, `breakpoints.1`)}) {
            padding: 0.5rem;
            font-size: 1rem;
          }

          @media screen and (min-width: ${get(t, `breakpoints.2`)}) {
            padding: 1rem;
            font-weight: bold;
          }
        `}
      >
        {person.name}
      </div>
    </div>
  )

  return result
}
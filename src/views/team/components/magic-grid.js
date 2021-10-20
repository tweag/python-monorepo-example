/** @jsx jsx */
import { jsx } from "theme-ui"
import { useState, useRef } from "react"
import {
  useResponsiveCallbacks,
  useAddEventListener,
} from "../hooks/magic-grid-hooks"
import ShuffleButton from "./shuffle-button"
import SearchBar from "./search-bar"

import { spawnTiles } from "./tileset"

import styles from "../styles/magic-grid.module.css"

/**
 * @param {{
 *  gap?: string,
 *  margin?: string,
 *  columns?: number,
 * }} props - gap and margins must contain valid css sizes.
 * @returns {{
 *  "--magic-grid-gap": string,
 *  "--magic-grid-margin": string,
 *  "--magic-grid-columns": number
 * } | {}}
 */
function parseCssVariables({ gap, margin, columns }) {
  const result = {}

  if (gap) {
    result[`--magic-grid-gap`] = gap
  }

  if (margin) {
    result[`--magic-grid-margin`] = margin
  }

  if (columns) {
    result[`--magic-grid-columns`] = columns
  }

  return result
}

const MagicGrid = ({ gap, margin, profiles, photos, tags }) => {
  // Setting up shuffle button
  // eslint-disable-next-line no-unused-vars
  const [shuffleState, reShuffle] = useState({})
  const [activeProfile, setActiveProfile] = useState(null)

  // Bio Events
  const mainRef = useRef()
  useAddEventListener(mainRef, `bio`, event => {
    const toActivate = event.tileInfo
    setActiveProfile(toActivate)
  })
  useAddEventListener(mainRef, `close-bio`, () => setActiveProfile(null))
  useAddEventListener(mainRef, `filter`, () => setActiveProfile(null))

  // Render parameters
  const [renderParameters, setParameters] = useState({
    color: Math.floor(profiles.length * 0.11),
    blank: Math.floor(profiles.length * 0.41),
    columns: 6,
  })

  const breakpoint = useResponsiveCallbacks({
    xs: [
      () =>
        setParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: 0,
          columns: 3,
        }),
    ],
    sm: [
      () =>
        setParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: 0,
          columns: 3,
        }),
    ],
    md: [
      () =>
        setParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
    lg: [
      () =>
        setParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
    xl: [
      () =>
        setParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
    xxl: [
      () =>
        setParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
  })

  // Parsing some CSS variables
  const sizingVariables = parseCssVariables({
    gap,
    margin,
    columns: renderParameters.columns,
  })
  return (
    <div className={styles.magicGridContainer} ref={mainRef}>
      <div
        className={styles.actionBar}
        sx={{
          pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          pr: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
        }}
      >
        <SearchBar placeholder={`Search for a name or a skill`} />
        <ShuffleButton onClick={() => reShuffle({})} />
      </div>
      <div className={styles.magicGrid} style={sizingVariables}>
        {spawnTiles({
          people: profiles,
          tags: tags,
          columns: renderParameters.columns,
          arbitraryAllocations: {
            color: renderParameters.color,
            blank: renderParameters.blank,
          },
          activeBioProfile: activeProfile,
          photos,
          breakpoint,
        })}
      </div>
    </div>
  )
}

export default MagicGrid

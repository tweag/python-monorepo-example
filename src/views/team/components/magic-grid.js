/** @jsx jsx */
import { jsx } from "theme-ui"
import { useState, useReducer } from "react"
import { useResponsiveCallbacks } from "../hooks/magic-grid-hooks"
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

const filterReducer = (state, action) => {
  const result = { ...state }
  const searchRegExp = new RegExp(action, `i`)

  result.profiles = result.initialProfiles.filter(profile => {
    const userTagMatches =
      profile.tags.filter(tag => searchRegExp.test(tag)).length > 0
    const nameMatches = searchRegExp.test(profile.name)
    const roleMatches = searchRegExp.test(profile.role)

    return userTagMatches || nameMatches || roleMatches
  })

  result.tags = result.initialTags.filter(tag => searchRegExp.test(tag))
  console.log(`Initial Tags: ${JSON.stringify(result.initialTags)}`)
  console.log(`Received filter: ${action}`)
  console.log(`Filtered tags: ${JSON.stringify(result.tags)}`)
  return result
}

const MagicGrid = ({ gap, margin, columns, profiles, photos, tags }) => {
  // Setting up the filter
  const [toRender, filter] = useReducer(filterReducer, {
    profiles,
    tags,
    initialProfiles: [...profiles],
    initialTags: [...tags],
  })

  // Setting up filter event handler
  // const mainRef = useRef()
  // useFilterEventWatcher(mainRef, filter)

  // Setting up shuffle button
  // eslint-disable-next-line no-unused-vars
  const [shuffleState, reShuffle] = useState({})

  // Render parameters
  const [renderParameters, setParameters] = useState({
    color: Math.floor(profiles.length * 0.11),
    blank: Math.floor(profiles.length * 0.41),
    columns: 6,
  })

  useResponsiveCallbacks({
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
    <div className={styles.magicGridContainer}>
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
          // toRender.profiles, photos, toRender.tags
          people: toRender.profiles,
          tags: toRender.tags,
          columns: renderParameters.columns,
          arbitraryAllocations: {
            color: renderParameters.color,
            blank: renderParameters.blank,
          },
          photos,
        })}
      </div>
    </div>
  )
}

export default MagicGrid

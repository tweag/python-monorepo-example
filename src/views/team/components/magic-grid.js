/** @jsx jsx */
import { jsx } from "theme-ui"
import { useState, useReducer, useRef } from "react"
import { useFilterEventWatcher } from "../hooks/magic-grid-hooks"

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

/**
 * @param {MouseEvent} event
 */
function shuffleButtonAnimationHandler(event) {
  const button = event.target
  button.classList.add(styles.rotateShuffleButton)
  button.addEventListener(
    `animationend`,
    () => button.classList.remove(styles.rotateShuffleButton),
    { once: true }
  )
}

const ShuffleButton = ({ onClick }) => {
  const clickEventHandler = event => {
    if (onClick) {
      onClick(event)
    }
    shuffleButtonAnimationHandler(event)
  }
  return (
    <a className={styles.shuffleButton} onClick={clickEventHandler}>
      Roll again
    </a>
  )
}

const SearchBar = ({ placeholder, filterCallback }) => {
  return (
    <input
      name="search"
      id="searchFilter"
      type="text"
      className={styles.searchBar}
      placeholder={placeholder}
      onInput={filterCallback}
    />
  )
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
  const filterHandler = event => {
    const filterString = event.target.value
    filter(filterString)
  }

  // Setting up filter event handler
  const mainRef = useRef()
  useFilterEventWatcher(mainRef, filter)

  // Setting up shuffle button
  // eslint-disable-next-line no-unused-vars
  const [shuffleState, reShuffle] = useState({})

  // Parsing some CSS variables
  const sizingVariables = parseCssVariables({ gap, margin, columns })
  return (
    <div className={styles.magicGridContainer} ref={mainRef}>
      <div
        className={styles.actionBar}
        sx={{
          pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          pr: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
        }}
      >
        <SearchBar
          placeholder={`Search for a name or a skill`}
          filterCallback={filterHandler}
        />
        <ShuffleButton onClick={() => reShuffle({})} />
      </div>
      <div className={styles.magicGrid} style={sizingVariables}>
        {spawnTiles(toRender.profiles, photos, toRender.tags)}
      </div>
    </div>
  )
}

export default MagicGrid

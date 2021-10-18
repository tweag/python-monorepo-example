/** @jsx jsx */
import { jsx } from "theme-ui"

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

const ShuffleButton = () => {
  return (
    <a className={styles.shuffleButton} onClick={shuffleButtonAnimationHandler}>
      Roll again
    </a>
  )
}

const SearchBar = ({ placeholder }) => {
  return (
    <input
      name="search"
      id="searchFilter"
      type="text"
      className={styles.searchBar}
      placeholder={placeholder}
    />
  )
}

const MagicGrid = ({ children, gap, margin, columns }) => {
  const sizingVariables = parseCssVariables({ gap, margin, columns })
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
        <ShuffleButton />
      </div>
      <div className={styles.magicGrid} style={sizingVariables}>
        {children}
      </div>
    </div>
  )
}

export default MagicGrid

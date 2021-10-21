/** @jsx jsx */
import { jsx } from "theme-ui"
import { useState, useRef } from "react"
import {
  useResponsiveCallbacks,
  findBreakpoint,
  useAddEventListener,
} from "../hooks/magic-grid-hooks"
import ShuffleButton from "./shuffle-button"
import SearchBar from "./search-bar"

import { TileSet } from "./tileset"
import { BioContext } from "./bio"

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
  const [uselessState, reRender] = useState({})

  // Render parameters
  const renderParameters = {
    color: Math.floor(profiles.length * 0.11),
    blank: Math.floor(profiles.length * 0.41),
    columns: 6,
  }

  let breakpoint = findBreakpoint().breakpoint

  if (breakpoint === `xs` || breakpoint === `sm`) {
    renderParameters.blank = 0
    renderParameters.columns = 3
  }

  // Parsing some CSS variables
  const sizingVariables = parseCssVariables({
    gap,
    margin,
    columns: renderParameters.columns,
  })

  // tile set
  const tileSetRef = useRef(
    new TileSet({
      people: profiles,
      tags: tags,
      columns: renderParameters.columns,
      arbitraryAllocations: {
        color: renderParameters.color,
        blank: renderParameters.blank,
      },
      activeBioProfile: null,
      photos,
      breakpoint,
    })
  )

  breakpoint = useResponsiveCallbacks({
    xs: [
      () =>
        tileSetRef.current.updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: 0,
          columns: 3,
        }),
    ],
    sm: [
      () =>
        tileSetRef.current.updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: 0,
          columns: 3,
        }),
    ],
    md: [
      () =>
        tileSetRef.current.updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
    lg: [
      () =>
        tileSetRef.current.updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
    xl: [
      () =>
        tileSetRef.current.updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
    xxl: [
      () =>
        tileSetRef.current.updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
        }),
    ],
  })

  // Bio Events
  const mainRef = useRef()
  const ajusterRef = useRef()
  useAddEventListener(mainRef, `toggle-bio`, event => {
    const toActivate = event.tileInfo
    if (tileSetRef.current.activeBioProfile) {
      tileSetRef.current.setActiveProfile(null)
    } else {
      tileSetRef.current.setActiveProfile(toActivate)
      if (breakpoint === `sm` || breakpoint === `xs`) {
        ajusterRef.current.scrollIntoView()
      }
    }
    reRender({})
  })
  useAddEventListener(mainRef, `filter`, () => {
    tileSetRef.current.setActiveProfile(null)
    reRender({})
  })

  // Shuffle Button
  const reShuffle = () => {
    tileSetRef.current.shuffleTiles()
    reRender({})
  }
  return (
    <div className={styles.magicGridContainer} ref={mainRef}>
      <div
        className={styles.actionBar}
        sx={{
          pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
          pr: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
        }}
        ref={ajusterRef}
      >
        <SearchBar placeholder={`Search for a name or a skill`} />
        <ShuffleButton onClick={() => reShuffle({})} />
      </div>
      <BioContext.Provider
        value={tileSetRef.current.activeBioProfile?.person?.slug ?? null}
      >
        <div className={styles.magicGrid} style={sizingVariables}>
          {tileSetRef.current.finalTiles}
        </div>
      </BioContext.Provider>
    </div>
  )
}

export default MagicGrid

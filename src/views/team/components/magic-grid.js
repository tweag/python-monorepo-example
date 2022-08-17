/** @jsx jsx */
import { jsx, useThemeUI } from "theme-ui"
import { useState, useRef, useEffect } from "react"

import {
  useResponsiveCallbacks,
  findBreakpoint,
  useAddEventListener,
} from "../hooks/magic-grid-hooks"
import ShuffleButton from "./shuffle-button"
import SearchBar from "./search-bar"

import { TileSet } from "./tileset"
import { BioContext } from "./bio"
import {
  SearchContext,
  useSearchManager,
  dispatchFilterEvent,
} from "../utils/search"

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
  // Setting up re-render function
  // eslint-disable-next-line no-unused-vars
  const [uselessState, reRender] = useState({})

  // Render parameters
  const renderParameters = {
    color: Math.floor(tags.length * 1.1),
    blank: Math.floor(tags.length * 3.6),
    columns: 6,
  }

  let breakpoint = findBreakpoint().breakpoint

  if (breakpoint === `xs` || breakpoint === `sm`) {
    renderParameters.blank = 0
    renderParameters.columns = 3
  }

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

  /**
   * @param {{
   *  color: number,
   *  blank: number,
   *  columns: number,
   *  breakpoint: string,
   * }} options
   */
  const updateResponsiveParameters = ({
    color,
    blank,
    columns,
    breakpoint,
  }) => {
    tileSetRef.current.updateResponsiveParameters({
      color,
      blank,
      columns,
      breakpoint,
    })
    reRender({})
  }

  breakpoint = useResponsiveCallbacks({
    xs: [
      () =>
        updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: 0,
          columns: 3,
          breakpoint: `xs`,
        }),
    ],
    sm: [
      () =>
        updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: 0,
          columns: 3,
          breakpoint: `sm`,
        }),
    ],
    md: [
      () =>
        updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 6,
          breakpoint: `md`,
        }),
    ],
    lg: [
      () =>
        updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 8,
          breakpoint: `lg`,
        }),
    ],
    xl: [
      () =>
        updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 8,
          breakpoint: `xl`,
        }),
    ],
    xxl: [
      () =>
        updateResponsiveParameters({
          color: Math.floor(profiles.length * 0.11),
          blank: Math.floor(profiles.length * 0.41),
          columns: 8,
          breakpoint: `xxl`,
        }),
    ],
  })

  // Bio Events
  const mainRef = useRef()
  const ajusterRef = useRef()
  const bioEventHandlerRef = useRef()
  const bioEventHandler = event => {
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
  }
  bioEventHandlerRef.current = bioEventHandler
  useAddEventListener(mainRef, `toggle-bio`, event =>
    bioEventHandlerRef.current(event)
  )
  useAddEventListener(mainRef, `filter`, () => {
    tileSetRef.current.setActiveProfile(null)
    reRender({})
  })
  useAddEventListener(mainRef, `bio-overflow`, () => {
    tileSetRef.current.updateBioHeight(tileSetRef.current.bioHeight + 1)
    reRender({})
  })

  // Parsing some CSS variables
  const sizingVariables = parseCssVariables({
    gap,
    margin,
    columns: tileSetRef.current.columns,
  })

  // Search setup
  const searchManager = useSearchManager(profiles)

  useEffect(() => {
    const reFilter = event => {
      const searchString = event.filterString ?? ``
      searchManager.filter(searchString)
    }

    window.addEventListener(`filter`, reFilter, { capture: true })
    return window.removeEventListener(`filter`, reFilter)
  }, [])

  // Shuffle Button click event handler
  const reShuffle = event => {
    const source = event.currentTarget
    tileSetRef.current.setActiveProfile(null)
    tileSetRef.current.shuffleTiles()

    searchManager.clear()
    dispatchFilterEvent(source, ``)
    reRender({})
  }

  const { theme: t } = useThemeUI()
  return (
    <SearchContext.Provider value={searchManager}>
      <div
        className="magicGridContainer"
        ref={mainRef}
        css={`
          display: grid;
          place-items: center;
          grid-template: "action-bar" "magic-grid" / 1fr;
          --magic-grid-max-width: 100rem;
        `}
      >
        <div
          className="actionBar"
          sx={{
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            pr: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            gridArea: `action-bar`,
            display: `flex`,
            flexDirection: [`column`, `column`, `row`],
            flexWrap: `nowrap`,
            justifyContent: `space-between`,
            width: `100%`,
            maxWidth: `var(--magic-grid-max-width)`,
          }}
          ref={ajusterRef}
        >
          <SearchBar placeholder={`Search for a name or a skill`} />
          <ShuffleButton
            className={
              (breakpoint === `xs` || breakpoint === `sm`) &&
              tileSetRef.current.activeBioProfile
                ? `hide`
                : ``
            }
            onClick={reShuffle}
          />
        </div>
        <BioContext.Provider
          value={tileSetRef.current.activeBioProfile?.person?.slug ?? null}
        >
          <div
            className="magicGrid"
            style={sizingVariables}
            css={`
              grid-area: magic-grid;
              justify-self: center;
              --magic-grid-width: min(var(--magic-grid-max-width), 100vw);
              width: var(--magic-grid-width);
              display: grid;
              padding: var(--magic-grid-margin, 1rem);
              gap: var(--magic-grid-gap, 1rem);
              grid-template-columns: repeat(var(--magic-grid-columns, 7), 1fr);
              transition: all 1s;
              --magic-grid-cell-width: calc(
                calc(
                    var(--magic-grid-width) -
                      calc(
                        calc(var(--magic-grid-margin, 1rem) * 2) +
                          calc(
                            var(--magic-grid-gap, 1rem) *
                              (var(--magic-grid-columns, 7) - 1)
                          )
                      )
                  ) / var(--magic-grid-columns, 7)
              );
              grid-auto-rows: var(--magic-grid-cell-width);
              grid-auto-columns: var(--magic-grid-cell-width);
              grid-auto-flow: row;
              --bio-rounding-amount: calc(var(--magic-grid-cell-width) * 0.4);
              animation: slowFadeIn 2s linear;
              animation-fill-mode: backwards;

              @media screen and (min-width: ${t.breakpoints[1]}) {
                --magic-grid-margin: 60px;
              }

              @media screen and (min-width: ${t.breakpoints[5]}) {
                --magic-grid-margin: 120px;
              }

              @keyframes slowFadeIn {
                from {
                  opacity: 0;
                }
                to {
                  opacity: 1;
                }
              }
            `}
          >
            {tileSetRef.current.finalTiles}
          </div>
        </BioContext.Provider>
      </div>
    </SearchContext.Provider>
  )
}

export default MagicGrid

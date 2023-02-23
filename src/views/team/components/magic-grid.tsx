import * as React from "react"
import { CSSProperties, get, useThemeUI, Box } from "theme-ui"
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
import { css } from "@emotion/react"
import { Person, TileInfo } from "./profile-tile"

function parseCssVariables({
  gap,
  margin,
  columns,
}: {
  gap?: string
  margin?: string
  columns?: number
}): CSSProperties {
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

const updateResponsiveParameters = ({
  tileSetRef,
  color,
  blank,
  columns,
  breakpoint,
}: {
  tileSetRef: React.MutableRefObject<TileSet>
  color: number
  blank: number
  columns: number
  breakpoint: string
}) => {
  tileSetRef.current.updateResponsiveParameters({
    color,
    blank,
    columns,
    breakpoint,
  })
}

const setResponsiveCallbacks = (
  tileSetRef: React.MutableRefObject<TileSet>,
  profiles: Person[],
  reRender: React.Dispatch<React.SetStateAction<object>>
) => {
  const profilesLengthTimes = (multiplier: number) =>
    Math.floor(profiles.length * multiplier)

  const defaultParams = {
    tileSetRef,
    color: profilesLengthTimes(0.11),
  }

  const fnWithReRender = (fn: () => void) => () => {
    fn()
    reRender({})
  }

  return useResponsiveCallbacks({
    xs: [
      fnWithReRender(() =>
        updateResponsiveParameters({
          ...defaultParams,
          blank: 0,
          columns: 3,
          breakpoint: `xs`,
        })
      ),
    ],
    sm: [
      fnWithReRender(() =>
        updateResponsiveParameters({
          ...defaultParams,
          blank: 0,
          columns: 3,
          breakpoint: `sm`,
        })
      ),
    ],
    md: [
      fnWithReRender(() =>
        updateResponsiveParameters({
          ...defaultParams,
          blank: profilesLengthTimes(0.41),
          columns: 6,
          breakpoint: `md`,
        })
      ),
    ],
    lg: [
      fnWithReRender(() =>
        updateResponsiveParameters({
          ...defaultParams,
          blank: profilesLengthTimes(0.41),
          columns: 8,
          breakpoint: `lg`,
        })
      ),
    ],
    xl: [
      fnWithReRender(() =>
        updateResponsiveParameters({
          ...defaultParams,
          blank: profilesLengthTimes(0.41),
          columns: 8,
          breakpoint: `xl`,
        })
      ),
    ],
    xxl: [
      fnWithReRender(() =>
        updateResponsiveParameters({
          ...defaultParams,
          blank: profilesLengthTimes(0.41),
          columns: 8,
          breakpoint: `xxl`,
        })
      ),
    ],
  })
}

type Props = {
  gap?: string
  margin?: string
  profiles: Person[]
  photos: Array<{ [slug: string]: string }>
  tags: string[]
}
const MagicGrid: React.FC<Props> = ({
  gap,
  margin,
  profiles,
  photos,
  tags,
}) => {
  // Setting up re-render function
  const [, reRender] = useState({})

  // Render parameters
  const renderParameters = {
    color: Math.floor(tags.length * 1.1),
    blank: Math.floor(tags.length * 3.6),
    columns: 6,
  }

  let breakpoint: string = findBreakpoint().breakpoint

  if (breakpoint === `xs` || breakpoint === `sm`) {
    renderParameters.blank = 0
    renderParameters.columns = 3
  }

  const toggleBio = (tileInfo?: TileInfo) => {
    if (tileSetRef.current.activeBioProfile) {
      tileSetRef.current.setActiveProfile(null)
      reRender({})
      return
    }

    tileSetRef.current.setActiveProfile(tileInfo)
    if (breakpoint === `sm` || breakpoint === `xs`) {
      ajusterRef.current?.scrollIntoView()
    }
    reRender({})
  }

  // tile set
  const tileSetRef = useRef<TileSet>(
    new TileSet({
      people: profiles,
      tags: tags,
      columns: renderParameters.columns,
      arbitraryAllocations: {
        color: renderParameters.color,
        blank: renderParameters.blank,
      },
      activeBioProfile: undefined,
      photos,
      breakpoint,
      onToggleBio: toggleBio,
    })
  )

  breakpoint = setResponsiveCallbacks(tileSetRef, profiles, reRender)

  // Bio Events
  const mainRef = useRef<HTMLDivElement>(null)
  const ajusterRef = useRef<HTMLDivElement>(null)
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
        css={css`
          display: grid;
          place-items: center;
          grid-template: "action-bar" "magic-grid" / 1fr;
          --magic-grid-max-width: 100rem;
        `}
      >
        <Box
          className="actionBar"
          sx={{
            pl: [`15px`, `60px`, `120px`],
            pr: [`15px`, `60px`, `120px`],
            gridArea: `action-bar`,
            display: `flex`,
            flexDirection: [`column`, `row`],
            flexWrap: `nowrap`,
            justifyContent: `space-between`,
            alignItems: `center`,
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
        </Box>
        <BioContext.Provider
          value={tileSetRef.current.activeBioProfile?.person?.slug ?? null}
        >
          <div
            className="magicGrid"
            style={sizingVariables}
            css={css`
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

              @media screen and (min-width: ${get(t, `breakpoints.1`)}) {
                --magic-grid-margin: 60px;
              }

              @media screen and (min-width: ${get(t, `breakpoints.2`)}) {
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

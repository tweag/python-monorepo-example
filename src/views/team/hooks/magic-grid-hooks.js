// eslint-disable-next-line no-unused-vars
import { useEffect, MutableRefObject, useState, useRef } from "react"

const BREAKPOINTS_RANGES = {
  xs: [0, 575],
  sm: [576, 767],
  md: [768, 991],
  lg: [992, 1199],
  xl: [1200, 1399],
  xxl: [1400, Infinity],
}

function findBreakpoint() {
  // Testing xs breakpoint
  let breakpoint = `xs`
  let mediaQuery = window.matchMedia(
    `screen and (min-width: ${BREAKPOINTS_RANGES[breakpoint][0]}px) and (max-width: ${BREAKPOINTS_RANGES[breakpoint][1]}px)`
  )

  if (mediaQuery.matches) {
    return { breakpoint, mediaQuery }
  }

  // testing sm breakpoint
  breakpoint = `sm`
  mediaQuery = window.matchMedia(
    `screen and (min-width: ${BREAKPOINTS_RANGES[breakpoint][0]}px) and (max-width: ${BREAKPOINTS_RANGES[breakpoint][1]}px)`
  )

  if (mediaQuery.matches) {
    return { breakpoint, mediaQuery }
  }

  // testing md breakpoint
  breakpoint = `md`
  mediaQuery = window.matchMedia(
    `screen and (min-width: ${BREAKPOINTS_RANGES[breakpoint][0]}px) and (max-width: ${BREAKPOINTS_RANGES[breakpoint][1]}px)`
  )

  if (mediaQuery.matches) {
    return { breakpoint, mediaQuery }
  }

  // testing lg breakpoint
  breakpoint = `lg`
  mediaQuery = window.matchMedia(
    `screen and (min-width: ${BREAKPOINTS_RANGES[breakpoint][0]}px) and (max-width: ${BREAKPOINTS_RANGES[breakpoint][1]}px)`
  )

  if (mediaQuery.matches) {
    return { breakpoint, mediaQuery }
  }

  // testing xl breakpoint
  breakpoint = `xl`
  mediaQuery = window.matchMedia(
    `screen and (min-width: ${BREAKPOINTS_RANGES[breakpoint][0]}px) and (max-width: ${BREAKPOINTS_RANGES[breakpoint][1]}px)`
  )

  if (mediaQuery.matches) {
    return { breakpoint, mediaQuery }
  }

  // testing xxl breakpoint
  breakpoint = `xxl`
  mediaQuery = window.matchMedia(
    `screen and (min-width: ${BREAKPOINTS_RANGES[breakpoint][0]}px)`
  )

  if (mediaQuery.matches) {
    return { breakpoint, mediaQuery }
  }

  // default case
  return {
    breakpoint: `md`,
    mediaQuery: window.matchMedia(
      `screen and (min-width: ${BREAKPOINTS_RANGES.md[0]}px) and (max-width: ${BREAKPOINTS_RANGES.md[1]}px)`
    ),
  }
}

/**
 * @param {MutableRefObject<HTMLElement>} mainRef
 * @param {string} eventName
 * @param {Function} eventHandler
 */
export function useAddEventListener(mainRef, eventName, eventHandler) {
  useEffect(() => {
    mainRef.current.addEventListener(eventName, eventHandler)
    return () => mainRef.current.removeEventListener(eventName, eventHandler)
  }, [])
}

/**
 * @param {
 *  xs?: (() => Function | void)[],
 *  sm?: (() => Function | void)[],
 *  md?: (() => Function | void)[],
 *  lg?: (() => Function | void)[],
 *  xl?: (() => Function | void)[],
 *  xxl?: (() => Function | void)[],
 * } callbacks
 */
export function useResponsiveCallbacks(callbacks) {
  const { breakpoint, mediaQuery } = findBreakpoint()
  const mediaQueryRef = useRef(mediaQuery)
  const breakpointRef = useRef(breakpoint)
  // eslint-disable-next-line no-unused-vars
  const [uselessState, triggerUpdate] = useState()
  useEffect(() => {
    mediaQueryRef.current.addEventListener(
      `change`,
      () => {
        const { breakpoint, mediaQuery } = findBreakpoint()
        mediaQueryRef.current = mediaQuery
        breakpointRef.current = breakpoint
        triggerUpdate({})
      },
      { once: true }
    )

    const cleanCallbacks = (callbacks[breakpointRef.current] ?? [])
      .map(callback => callback())
      .filter(callback => !!callback)

    return () => cleanCallbacks.forEach(callback => callback())
  }, [breakpointRef.current])

  return breakpointRef.current
}

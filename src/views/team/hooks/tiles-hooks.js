import { useEffect, useState } from "react"

/**
 * @param {(filterString: string) => boolean} filteringHandler
 */
export function useFilteredMode(filteringHandler) {
  const [display, setVisibility] = useState(true)
  const filterHandler = event => {
    setVisibility(filteringHandler(event.filterString))
  }

  useEffect(() => {
    window.addEventListener(`filter`, filterHandler, { capture: true })
    return window.removeEventListener(`filter`, filterHandler)
  }, [])

  return display
}

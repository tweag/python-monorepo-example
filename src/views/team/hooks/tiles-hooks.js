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
    window.document.body.addEventListener(`filter`, filterHandler)
    return window.document.body.removeEventListener(`filter`, filterHandler)
  }, [])

  return display
}

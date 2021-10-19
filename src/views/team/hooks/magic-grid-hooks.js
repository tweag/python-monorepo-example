// eslint-disable-next-line no-unused-vars
import { useEffect, MutableRefObject } from "react"

/**
 * @param {MutableRefObject<HTMLElement>} mainRef
 * @param {(filterString: string) => void} filterCallback
 */
export function useFilterEventWatcher(mainRef, filterCallback) {
  const onFilter = event => filterCallback(event.filterString)
  useEffect(() => {
    mainRef.current.addEventListener(`filter`, onFilter)
    return () => mainRef.current.removeEventListener(`filter`, onFilter)
  }, [])
}

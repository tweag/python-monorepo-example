// eslint-disable-next-line no-unused-vars
import { useEffect, MutableRefObject } from "react"

/**
 * Handler to enable event watching for the custom toggle-fold event
 * @param {MutableRefObject<HTMLElement>} accordionRef
 * @param {(event: Event) => void} handler
 */
export function useToggleEventMonitor(accordionRef, handler) {
  useEffect(() => {
    const accordion = accordionRef.current
    accordion.addEventListener(`accordion-item-toggle-folding`, handler, {
      capture: true,
    })
    return () => {
      accordion.removeEventListener(`accordion-item-toggle-folding`, handler)
    }
  }, [])
  console.log(`Accordion listening toogle event`)
}

// eslint-disable-next-line no-unused-vars
import { useEffect, MutableRefObject } from "react"

/**
 * @param {MutableRefObject<EventTarget>} ref
 * @param {string} eventName
 * @param {(event: Event) => void} handler
 */
export default function useEventListener(ref, eventName, handler) {
  useEffect(() => {
    const target = ref.current
    target.addEventListener(eventName, handler)
    return target.removeEventListener(eventName, handler)
  }, [])
}

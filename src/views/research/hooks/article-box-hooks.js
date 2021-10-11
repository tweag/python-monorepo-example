// eslint-disable-next-line no-unused-vars
import { useEffect, MutableRefObject } from "react"

import styles from "../styles/accordion.module.css"

/**
 * @param {MutableRefObject<HTMLElement>} ref
 * @param {Function} callback
 */
export function useTagToggledEventMonitor(ref, callback) {
  useEffect(() => {
    const element = ref.current
    element.addEventListener(`toggled-tag`, callback)
    return () => element.removeEventListener(`toggled-tag`, callback)
  }, [])
}

/**
 * @param {MutableRefObject<HTMLElement>} ref
 */
export function useFirstArticleToggler(ref) {
  useEffect(() => {
    const articleBox = ref.current
    const firstArticle = articleBox.querySelector(`[data-accordion-item]`)
    firstArticle.classList.add(styles.active)
  }, [])
}

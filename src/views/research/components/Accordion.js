/** @jsx jsx */
import { jsx } from "theme-ui"
import { useRef } from "react"

import { useToggleEventMonitor } from "../hooks/accordion-hooks"

import * as styles from "../styles/accordion.module.css"

/**
 * Event handler for click event that should toggle folding on accordion item
 * @param {MouseEvent} event - Click Event
 */
function accordionItemClickHandler(event) {
  const currentElement = event.target
  currentElement.dispatchEvent(
    new Event(`accordion-item-toggle-folding`, { bubbles: true })
  )
  console.log(`Event dispatched`)
}

/**
 * Handler to be executed whe the accordion needs to toggle an item
 * @param {Event} event
 */
function accordionToggleHandler(event) {
  console.log(`Event received`)
  const accordion = event.currentTarget
  const itemToActivate = event.target.closest(`[data-accordion-item]`)
  const accordionItems = accordion.querySelectorAll(`[data-accordion-item]`)

  for (const accordionItem of accordionItems) {
    accordionItem.classList.remove(styles.active)
  }

  itemToActivate.classList.add(styles.active)
}

export const Accordion = ({ children, className, style }) => {
  const accordionRef = useRef()
  useToggleEventMonitor(accordionRef, accordionToggleHandler)

  return (
    <div
      ref={accordionRef}
      className={`${styles.accordion} ${className ?? ``}`}
      style={style ?? {}}
    >
      {children}
    </div>
  )
}

/**
 * Item to be put inside an Accordion Component
 * @param {{
 *  visibleContent: JSX.Element,
 *  invisibleContent: JSX.Element,
 *  className?: string,
 *  key?: string | number
 * }} props - An object containing the following properties:
 *  - `visibleContent: JSX.Element`: An element with the things that will be visible while the element is collapsed.
 *  - `invisibleContent: JSX.Element`: An element with the things that will only be visible when the Item is expanded.
 * @returns {JSX.Element}
 */
export const AccordionItem = ({
  visibleContent,
  invisibleContent,
  colorOnExpand,
  className,
  style,
}) => {
  return (
    <div
      data-accordion-item={true}
      data-color-on-expand={colorOnExpand ? `yes` : `no`}
      className={`${styles.accordionItem} ${className ?? ``}`}
      style={style ?? {}}
      onClick={accordionItemClickHandler}
    >
      <div className={styles.visibleContent}>{visibleContent}</div>
      <div className={styles.invisibleContent}>{invisibleContent}</div>
    </div>
  )
}

export default Accordion

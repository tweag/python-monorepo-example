import React from "react"
import styles from "../styles/shuffle-button.module.css"

/**
 * @param {MouseEvent} event
 */
function shuffleButtonAnimationHandler(event) {
  const button = event.target
  button.classList.add(styles.rotateShuffleButton)
  button.addEventListener(
    `animationend`,
    () => button.classList.remove(styles.rotateShuffleButton),
    { once: true }
  )
}

const ShuffleButton = ({ onClick, ref }) => {
  const clickEventHandler = event => {
    if (onClick) {
      onClick(event)
    }
    shuffleButtonAnimationHandler(event)
  }
  return (
    <a className={styles.shuffleButton} ref={ref} onClick={clickEventHandler}>
      Roll again
    </a>
  )
}

export default ShuffleButton

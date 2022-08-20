/** @jsx jsx */
import { jsx } from "theme-ui"

import * as styles from "../styles/loading-animation.module.css"

const LoadingAnimation = () => {
  return (
    <div className={styles.externalContainer}>
      <div className={styles.dotsContainer}>
        <div className={styles.firstDot}></div>
        <div className={styles.secondDot}></div>
        <div className={styles.thirdDot}></div>
      </div>
    </div>
  )
}

export { LoadingAnimation }

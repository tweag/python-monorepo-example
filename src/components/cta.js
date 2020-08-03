import React from "react"
import { Link } from "gatsby"

const CallToAction = ({ areaIndex, backdropIndex }) => {
  const areaClass = `vision-area${areaIndex || backdropIndex} animation-wrap`
  const backdropClass = `image-holder backdrop${backdropIndex}`
  return (
    <div className={areaClass}>
      <div className="container">
        <div className="vision-holder">
          <div className="text-col">
            <h2>Ready to achieve your big vision?</h2>
            <Link to="/contact" className="btn">
              Contact us
            </Link>
          </div>
          <div className={backdropClass}></div>
        </div>
      </div>
    </div>
  )
}

export default CallToAction

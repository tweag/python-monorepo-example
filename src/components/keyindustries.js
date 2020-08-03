import React from "react"
import { Link } from "gatsby"

import biotech from "../images/img3.gif"
import fintech from "../images/img4.gif"
import vehicles from "../images/img5.gif"

const KeyIndustries = ({ isFullPage }) => {
  const containerClasses = isFullPage ? `container fullp` : `container`
  return (
    <div className="section s_white section-wrap key_indus viewport-section">
      <div className={containerClasses}>
        <div className="col-area animation-wrap">
          <h2>Key industries we serve</h2>
          <div className="col-row">
            <div className="info-col">
              <div className="block">
                <div className="image-holder">
                  <img src={biotech} alt="Biotech" />
                </div>
                <div className="description">
                  <h3>Biotech</h3>
                  <p>
                    Build statistical models, iterate on them quickly and
                    increase productivity.
                  </p>
                  <Link to="/industry/biotech" className="btn">
                    Learn more
                  </Link>
                </div>
              </div>
            </div>
            <div className="info-col">
              <div className="block">
                <div className="image-holder">
                  <img src={fintech} alt="Fintech" />
                </div>
                <div className="description">
                  <h3>Fintech</h3>
                  <p>
                    Minimize risk with high-assurance software, from blockchain
                    to trading systems.
                  </p>
                  <Link to="/industry/fintech" className="btn">
                    Learn more
                  </Link>
                </div>
              </div>
            </div>
            <div className="info-col">
              <div className="block">
                <div className="image-holder">
                  <img src={vehicles} alt="Autonomous vehicles" />
                </div>
                <div className="description">
                  <h3>Autonomous vehicles</h3>
                  <p>
                    Put safety first with robust and reliable software, backed
                    by static analysis.
                  </p>
                  <Link to="/industry/autonomous" className="btn">
                    Learn more
                  </Link>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}

export default KeyIndustries

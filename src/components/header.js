import React from "react"
import { Link } from "gatsby"

import JQuery from "./jquery"

import blackLogo from "../images/logo_tweag_black.svg"
import whiteLogo from "../images/logo_tweag_white.svg"

const Header = () => {
  return (
    <header className="header">
      <JQuery />
      <div className="header-holder">
        <strong className="logo">
          <Link to="/">
            <img className="black-logo" src={blackLogo} alt="Tweag" />
          </Link>
          <Link to="/">
            <img className="white-logo" src={whiteLogo} alt="Tweag" />
          </Link>
        </strong>
        <div className="nav-drop">
          <div className="nav-area">
            <ul className="menu">
              <li>
                <Link to="/services">Services</Link>
              </li>
              <li className="active">
                <a href="#top">Key industries</a>
                <div className="dropdown-menu">
                  <ul className="dropdown-list">
                    <li>
                      <Link to="/industry/biotech">Biotech</Link>
                    </li>
                    <li>
                      <Link to="/industry/fintech">Fintech</Link>
                    </li>
                    <li>
                      <Link to="/industry/autonomous">Autonomous vehicles</Link>
                    </li>
                  </ul>
                </div>
              </li>
              <li>
                <Link to="/opensource">Open source</Link>
              </li>
              <li>
                <Link to="/contact">Contact</Link>
              </li>
              <li>
                <Link to="/blog">Blog</Link>
              </li>
            </ul>
          </div>
        </div>
        <a className="menu-opener" href="#top">
          {" "}
        </a>
      </div>
    </header>
  )
}

export default Header

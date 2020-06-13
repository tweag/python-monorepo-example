/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link } from "gatsby"

import blackLogo from "../images/logo_tweag_black.svg"
import whiteLogo from "../images/logo_tweag_white.svg"

const Logo = () => (
  <strong
    className="logo"
    sx={{
      position: `relative`,
      transform: [null, null, `scale(0.9)`],
      mt: [null, null, `-14px`],
      transition: `all 0.4s ease`,
      width: [`175px`, `275px`, null, null, null, `300px`],
      display: `block`,
      pb: `10px`,
      "& a": {
        display: `block`,
      },
      "& img": {
        display: `block`,
        width: `100%`,
        height: `auto`,
        transition: `all 0.4s ease 0.4s`,
      },
    }}
  >
    <Link to="/">
      <img className="black-logo" src={blackLogo} alt="Tweag" />
    </Link>
    <Link to="/">
      <img
        sx={{
          position: `absolute`,
          left: 0,
          top: 0,
          opacity: 0,
          visibility: `hidden`,
        }}
        className="white-logo"
        src={whiteLogo}
        alt="Tweag"
      />
    </Link>
  </strong>
)

const Navigation = () => (
  <header
    className="header"
    sx={{
      position: `fixed`,
      left: 0,
      right: 0,
      top: 0,
      height: `50px`,
      p: [`15px`, `25px`, null, null, null, `35px`],
      zIndex: 99,
      transition: `all 0.3s`,
    }}
  >
    <div
      sx={{
        position: [`static`, `relative`],
        display: [`block`, `flex`],
        flexWrap: `wrap`,
        alignItems: `center`,
        justifyContent: `space-between`,
      }}
    >
      <Logo />
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
        {` `}
      </a>
    </div>
  </header>
)

export default Navigation

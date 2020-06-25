/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link } from "gatsby"

import NavList from "./navlist"
import NavItem from "./navitem"

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

const DropdownMenu = ({ children }) => (
  <div
    className="dropdown-menu"
    sx={{
      transition: `all 0.4s ease`,
      position: [`relative`, `absolute`],
      left: 0,
      right: [0, `unset`],
      top: [0, `100%`],
      minWidth: [`inherit`, `300px`],
      p: [0, `10px 0 0`],
      opacity: [1, 0],
      visibility: [`visible`, `hidden`],
      display: [`none`, `unset`],
      fontWeight: 400,
      zIndex: 5,
    }}
  >
    <NavList
      className="dropdown-list"
      sx={{
        ml: `-10px`,
        background: `white`,
        p: `10px`,
        pb: [0, `10px`],
        fontSize: [`75%`, `unset`],
        li: {
          position: `relative`,
          mb: `10px`,
        },
        "li::last-child": {
          mb: 0,
        },
      }}
    >
      {children}
    </NavList>
  </div>
)

const Menu = ({ children }) => (
  <div
    className="nav-drop"
    sx={{
      position: `relative`,
      "@media screen and (max-width: 767px)": {
        transition: `all 0.4s ease`,
        position: `absolute`,
        left: 0,
        top: `100%`,
        width: `100%`,
        overflow: `hidden`,
        maxHeight: 0,
        zIndex: 99,
      },
    }}
  >
    <div
      className="nav-area"
      sx={{
        position: `relative`,
        pt: `16px`,
        "@media screen and (max-width: 767px)": {
          transform: `translateY(-100%)`,
          transition: `all 0.4s ease`,
        },
      }}
    >
      <NavList
        className="menu"
        sx={{
          background: [`white`, `unset`],
          mx: [`-8px`, `-15px`],
          mb: 0,
          mt: [0, null, `-10px`],
          px: [`15px`, `unset`],
          py: [`50px`, `unset`],
          height: [`100vh`, `unset`],
          transform: [null, null, `scale(0.9)`],
          fontSize: [4, 1, null, null, null, 2],
          lineHeight: [1.8, 1.1],
          display: [`block`, `flex`],
          textAlign: [`center`, `unset`],
          flexWrap: `wrap`,
          alignItems: `center`,
          transition: `all 0.4s ease`,
          "*::selection": {
            backgroundColor: `rgba(255, 255, 255, 0) !important`,
          },
          "*::-moz-selection": {
            backgroundColor: `rgba(255, 255, 255, 0) !important`,
          },
          "& > li": {
            position: `relative`,
            m: [`0 15px`, null, null, null, null, `0 25px`],
            mb: [`15px`, 0],
            transition: `all 0.4s ease`,
          },
          "& > li:hover .dropdown-menu": {
            opacity: 1,
            visibility: `visible`,
            display: [`block`, `unset`],
          },
        }}
      >
        {children}
      </NavList>
    </div>
  </div>
)

const MobileMenuOpener = () => (
  <a
    className="menu-opener"
    href="#top"
    sx={{
      position: `absolute`,
      top: `15px`,
      right: `15px`,
      borderTop: `2px solid black`,
      transition: `all 0.4s ease`,
      width: `30px`,
      height: `20px`,
      display: [`block`, `none`],
      "&::before, &::after": {
        width: `30px`,
        height: `2px`,
        left: 0,
        transition: `all 0.4s ease`,
        content: `""`,
        position: `absolute`,
        background: `black`,
      },
      "&::before": {
        top: `6px`,
      },
      "&::after": {
        top: `15px`,
      },
    }}
  >
    {` `}
  </a>
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
      <Menu>
        <NavItem>
          <Link to="/services">Services</Link>
        </NavItem>
        <NavItem className="active">
          <a href="#top">Key industries</a>
          <DropdownMenu>
            <NavItem>
              <Link to="/industry/biotech">Biotech</Link>
            </NavItem>
            <NavItem>
              <Link to="/industry/fintech">Fintech</Link>
            </NavItem>
            <NavItem>
              <Link to="/industry/autonomous">Autonomous vehicles</Link>
            </NavItem>
          </DropdownMenu>
        </NavItem>
        <NavItem>
          <Link to="/opensource">Open source</Link>
        </NavItem>
        <NavItem>
          <Link to="/contact">Contact</Link>
        </NavItem>
        <NavItem>
          <Link to="/blog">Blog</Link>
        </NavItem>
      </Menu>
      <MobileMenuOpener />
    </div>
  </header>
)

export default Navigation
/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link, navigate } from "gatsby"

import NavList from "./navlist"
import NavItem from "./navitem"

import blackLogo from "../images/logo_tweag_black.svg"

const Logo = () => (
  <strong
    className="logo"
    sx={{
      position: `relative`,
      transform: [null, null, null, `scale(0.9)`],
      mt: [null, null, null, `-14px`],
      transition: `all 0.4s ease`,
      width: [`175px`, `175px`, `275px`, null, null, null, `300px`],
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
  </strong>
)

const DropdownMenu = ({ children }) => (
  <div
    className="dropdown-menu"
    sx={{
      transition: `all 0.4s ease`,
      position: [`relative`, `relative`, `absolute`],
      left: 0,
      right: [0, 0, `unset`],
      top: [0, 0, `100%`],
      minWidth: [`inherit`, `inherit`, `300px`],
      p: [0, 0, `10px 0 0`],
      opacity: [1, 1, 0],
      visibility: [`visible`, `visible`, `hidden`],
      display: [`none`, `none`, `unset`],
      fontWeight: 400,
      zIndex: 5,
    }}
  >
    <NavList
      className="dropdown-list"
      sx={{
        ml: `-10px`,
        background: `var(--bg-color)`,
        p: `10px`,
        pb: [0, 0, `10px`],
        fontSize: [`75%`, `75%`, `unset`],
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
          mx: [`-8px`, `-8px`, `-15px`],
          mb: 0,
          mt: [0, 0, null, `-10px`],
          px: [`15px`, `15px`, `unset`],
          py: [`50px`, `50px`, `unset`],
          height: [`100vh`, `100vh`, `unset`],
          transform: [null, null, null, `scale(0.9)`],
          fontSize: [4, 4, 1, null, null, null, 2],
          lineHeight: [1.8, 1.8, 1.1],
          display: [`block`, `block`, `flex`],
          textAlign: [`center`, `center`, `unset`],
          flexWrap: `wrap`,
          alignItems: `center`,
          transition: `all 0.4s ease`,
          "& > li": {
            position: `relative`,
            m: [`0 15px`, `0 15px`, null, null, null, null, `0 25px`],
            mb: [`15px`, `15px`, 0],
            transition: `all 0.4s ease`,
          },
          "& > li:hover .dropdown-menu": {
            opacity: 1,
            visibility: `visible`,
            display: [`block`, `block`, `unset`],
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
      borderTop: `2px solid var(--fg-color)`,
      transition: `all 0.4s ease`,
      width: `30px`,
      height: `20px`,
      display: [`block`, `block`, `none`],
      ".menu-active &": {
        borderTop: `none`,
      },
      "&::before, &::after": {
        width: `30px`,
        height: `2px`,
        left: 0,
        transition: `all 0.4s ease`,
        content: `""`,
        position: `absolute`,
        background: `var(--fg-color)`,
      },
      "&::before": {
        top: `6px`,
      },
      "&::after": {
        top: `15px`,
      },
      ".menu-active &::before": {
        transform: `rotate(45deg)`,
        top: `10px`,
      },
      ".menu-active &::after": {
        transform: `rotate(-45deg)`,
        top: `10px`,
      },
      ".header-inverse &": {
        borderTopColor: `white`,
      },
      ".header-inverse &::after, .header-inverse &::after": {
        background: `white`,
      },
    }}
  >
    {` `}
  </a>
)

const MyLink = ({ children, href }) => {
  const handleNavClick = e => {
    e.preventDefault()
    const pageLink = e.target.getAttribute(`href`)
    navigate(pageLink)
    const body = document.getElementsByTagName(`body`)[0]
    const menuActiveClass = `menu-active`
    if (body.classList.contains(menuActiveClass))
      body.classList.remove(menuActiveClass)
  }
  return (
    <NavItem>
      <a href={href} onClick={handleNavClick}>
        {children}
      </a>
    </NavItem>
  )
}

const Navigation = ({ className }) => (
  <header
    className={`header ` + (className ? className : ``)}
    sx={{
      position: `fixed`,
      left: 0,
      right: 0,
      top: 0,
      minHeight: `50px`,
      p: [`15px`, `15px`, `25px`, null, null, null, `35px`],
      zIndex: 99,
      transition: `all 0.3s`,
    }}
  >
    <div
      sx={{
        position: [`static`, `static`, `relative`],
        display: [`block`, `block`, `flex`],
        flexWrap: `wrap`,
        alignItems: `center`,
        justifyContent: `space-between`,
      }}
    >
      <Logo />
      <Menu>
        <MyLink href="/services">Services </MyLink>
        <NavItem className="active">
          <a href="#top">Key industries</a>
          <DropdownMenu>
            <MyLink href="/industry/biotech">Biotech</MyLink>
            <MyLink href="/industry/fintech">Fintech</MyLink>
            <MyLink href="/industry/autonomous">Autonomous vehicles</MyLink>
          </DropdownMenu>
        </NavItem>
        <MyLink href="/opensource">Open source</MyLink>
        <MyLink href="/contact">Contact</MyLink>
        <MyLink href="/careers">Careers</MyLink>
        <MyLink href="/blog">Blog</MyLink>
      </Menu>
      <MobileMenuOpener />
    </div>
  </header>
)

export default Navigation

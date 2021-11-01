/** @jsx jsx */
import { jsx, Flex, Box, Text } from "theme-ui"
import { useState, useEffect, useRef } from "react"
import ClickAwayListener from "react-click-away-listener"
import { Link } from "gatsby"
import { globalHistory } from "@reach/router"
import blackLogo from "../images/logo_tweag_black.svg"

const Logo = () => (
  <Link to="/">
    <img
      sx={{
        width: `100%`,
        height: `auto`,
        transition: `filter 0.4s ease`,
        ".navbar-inverted &": {
          filter: `invert(100%)`,
        },
      }}
      className="black-logo"
      src={blackLogo}
      alt="Tweag"
    />
  </Link>
)

const ExternalLink = ({ to, children, ...restProps }) => (
  <a {...restProps} href={to}>
    {children}
  </a>
)

const NavLink = ({
  children,
  to,
  customSx,
  customClassName,
  isExternal = false,
}) => {
  const Ele = isExternal ? ExternalLink : Link
  return (
    <Ele
      to={to}
      sx={{
        color: `black`,
        ".navbar-inverted &": {
          color: `white !important`,
          "::after": {
            bg: `white`,
          },
        },
        transition: `all 0.4s ease`,
        fontSize: [`27px`, `27px`, `16px`, null, null, null, `18px`],
        lineHeight: [1.8, 1.8, 1.1],
        "&:visited": {
          color: `black`,
        },
        minWidth: [`fit-content`],
        ...customSx,
      }}
      className={customClassName}
    >
      {children}
    </Ele>
  )
}

const MobileMenuOpener = ({ onClick }) => (
  <a
    onClick={onClick}
    sx={{
      position: `absolute`,
      top: `15px`,
      right: `15px`,
      borderTop: `2px solid var(--fg-color)`,
      transition: `all 0.4s ease`,
      width: `30px`,
      height: `20px`,
      display: [`block`, `block`, `none`],
      cursor: `pointer`,
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
    }}
  >
    {` `}
  </a>
)

function Header({ inverted, fullpage = false }) {
  const headerRef = useRef(null)
  const dropDownKeyIndustriesEle = useRef(null)
  const [navbarState, setNavbarState] = useState(false)

  useEffect(() => {
    return globalHistory.listen(({ action }) => {
      if (action === `PUSH`) {
        setNavbarState(false)
        hideDropDown()
      }
    })
  }, [])

  useEffect(() => {
    const headerEle = headerRef.current
    if (navbarState) {
      headerEle.classList.add(`in-viewport`, `menu-active`)
    } else {
      headerEle.classList.remove(`in-viewport`, `menu-active`)
    }
  }, [navbarState])

  const toggleNav = () => setNavbarState(p => !p)

  const navLinkClassName = `button min-1__button-link-bottom-lined`

  const hideDropDown = () => {
    if (!dropDownKeyIndustriesEle.current) return
    dropDownKeyIndustriesEle.current.classList.remove(
      `header-drop-down-transition__show-in--on`
    )
  }

  const setDropDownVisible = () => {
    if (!dropDownKeyIndustriesEle.current) return
    dropDownKeyIndustriesEle.current.classList.add(
      `header-drop-down-transition__show-in--on`
    )
  }

  return (
    <Flex
      ref={headerRef}
      sx={{
        flexWrap: `wrap`,
        p: [`15px`, `15px`, `25px`, null, null, null, `35px`],
        justifyContent: [`space-between`],
        alignItems: [`center`, null, `flex-end`],
        position: `fixed`,
        top: 0,
        right: 0,
        left: 0,
        zIndex: 99,
        mt: [0, 0, `-16px`, fullpage ? `-14px` : null],
        transition: `all 0.4s ease`,
      }}
      className={`transition-section header ${inverted && `navbar-inverted`}`}
    >
      <Box
        sx={{
          width: [`175px`, `175px`, `275px`, null, null, null, `300px`],
          mt: [0, 0, `16px`, fullpage ? 0 : null],
          pb: [0, 0, `7px`],
          transform: [null, null, null, fullpage ? `scale(0.9)` : null],
        }}
      >
        <Logo />
      </Box>
      <Flex
        className={`transition-section__transition--slide-fade-in top-in duration-3 delayed-0 min-1--none`}
        sx={{
          flexDirection: [`column`, null, `row`],
          flexBasis: [`100%`, `100%`, `auto`],
          ml: [null, null, null, `auto`],
          mt: [0, 0, `16px`, fullpage ? 0 : null],
          alignItems: [`center`, `center`, `flex-end`],
          height: [`100vh`, null, `auto`],
          opacity: [navbarState ? 1 : 0, navbarState ? 1 : 0, 1],
          zIndex: [navbarState ? 99 : -999, navbarState ? 99 : -999, 99],
          position: [`absolute`, null, `unset`],
          top: [`45px`, null, 0],
          right: [0],
          left: [0],
          pt: [`66px`, null, 0],
          background: [`var(--bg-color)`, null, `inherit`],
        }}
      >
        <NavLink
          to="/services"
          customSx={{
            mx: [0, 0, `15px`, null, `15px`, null, null, null, `25px`],
            mr: [0, 0, `15px`, null, `15px`, null, null, null, `25px`],
            ml: [0],
            mb: [`15px`, `15px`, 0],
          }}
          customClassName={navLinkClassName}
        >
          Services
        </NavLink>
        <ClickAwayListener onClickAway={() => hideDropDown(`key-industries`)}>
          <Box
            onMouseEnter={() => setDropDownVisible(`key-industries`)}
            onMouseLeave={() => hideDropDown(`key-industries`)}
            onClick={() => setDropDownVisible(`key-industries`)}
            sx={{
              mx: [0, 0, `15px`, null, `15px`, null, null, null, `25px`],
              mb: [`15px`, `15px`, 0],
              textAlign: [`center`, `center`, `start`],
              bg: `inherit`,
              ".header-drop-down-transition__show-in": {
                display: [`none`, `none`, `flex`],
                opacity: 0,
                transform: [null, null, `translateY(-1000px)`],
                transition: [
                  null,
                  null,
                  `opacity 0.4s ease, transform 0.1s ease 0.4s`,
                ],
              },
              ".header-drop-down-transition__show-in--on": {
                display: `flex`,
                opacity: 1,
                transform: [null, null, `translateY(0px)`],
                transition: [null, null, `opacity 0.4s ease`],
              },
            }}
          >
            <Text
              sx={{
                minWidth: [`fit-content`],
                fontSize: [`27px`, `27px`, `16px`, null, null, null, `18px`],
                lineHeight: [1.1],
                cursor: `pointer`,
                userSelect: `none`,
                transition: `all 0.4s ease`,
                color: `black`,
                ".navbar-inverted &": {
                  color: `white !important`,
                  "::after": {
                    bg: `white`,
                  },
                },
              }}
              className={navLinkClassName}
            >
              Key industries
            </Text>
            <Flex
              ref={dropDownKeyIndustriesEle}
              className={`header-drop-down-transition__show-in`}
              sx={{
                bg: `var(--bg-color)`,
                ".navbar-inverted &": {
                  bg: `transparent`,
                },
                position: [null, null, `absolute`],
                flexDirection: `column`,
                pt: [`15px`, `15px`, `10px`],
                pb: [0, 0, `10px`],
                mx: [`-15px`],
                px: [`15px`],
                alignItems: [`center`, `center`, `start`],
              }}
            >
              {[
                [`Biotech`, `biotech`],
                [`Fintech`, `fintech`],
                [`Autonomous Vehicles`, `autonomous`],
              ].map(([t, route], i) => (
                <NavLink
                  key={t}
                  to={`/industry/${route}`}
                  customSx={{
                    mb: `10px`,
                    fontSize: [
                      `23px`,
                      `23px`,
                      `16px`,
                      null,
                      null,
                      null,
                      `18px`,
                    ],
                  }}
                  customClassName={navLinkClassName}
                >
                  {t}
                </NavLink>
              ))}
            </Flex>
          </Box>
        </ClickAwayListener>
        {[
          [`Open source`, `/opensource`],
          [`Contact`, `/contact`],
          [`Careers`, `//boards.greenhouse.io/tweag`, true],
          [`Research`, `/research`],
          [`Blog`, `/blog`],
        ].map(([t, route, isExternal], i, arr) => (
          <NavLink
            customClassName={navLinkClassName}
            key={t}
            to={route}
            isExternal={isExternal}
            customSx={{
              mx: [0, 0, `15px`, null, `15px`, null, null, null, `25px`],
              ...(i === arr.length - 1 ? { mr: [0, 0, 0, 0, 0, 0, 0, 0] } : {}),
              minWidth: [`fit-content`],
              mb: [`15px`, `15px`, 0],
            }}
          >
            {t}
          </NavLink>
        ))}
      </Flex>
      <MobileMenuOpener onClick={toggleNav} />
    </Flex>
  )
}

export default Header

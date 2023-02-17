/** @jsx jsx */
import React from "react"
import { Flex, Box, Text, jsx } from "theme-ui"
import { useState, useEffect, useRef } from "react"
import { Link } from "gatsby"
import blackLogo from "../images/logo_tweag_modus_header.png"

const navLinkClassName = `button min-1__button-link-bottom-lined`
const menuItemFontSize = [`27px`, `1.5vw`, `1.3vw`]

const Logo = () => (
  <Link to="/">
    <img
      sx={{
        width: `300px`,
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

type DropDownProps = {
  title: string
  items: Array<{ title: string; to: string }>
}
const Dropdown: React.FC<DropDownProps> = ({ title, items }) => {
  const dropDownClass = `header-drop-down-transition__show-in`
  return (
    <Box
      sx={{
        mx: [0, `15px`, `25px`],
        mb: [`15px`, 0],
        textAlign: [`center`, `start`],
        bg: `inherit`,
          display: [`none`, `flex`],
        [`.${dropDownClass}`]: {
          opacity: 0,
          transform: [null, `translateY(-1000px)`],
          transition: [null, `opacity 0.4s ease, transform 0.1s ease 0.4s`],
        },
          display: `flex`,
        [`&:hover .${dropDownClass}, &:focus-within .${dropDownClass}`]: {
          opacity: 1,
          transform: [null, `translateY(0px)`],
          transition: [null, `opacity 0.4s ease`],
        },
      }}
    >
      <Text
        as="div"
        sx={{
          minWidth: [`fit-content`],
          lineHeight: [1.1],
          fontSize: menuItemFontSize,
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
        {title}
      </Text>
      <Flex
        className={dropDownClass}
        sx={{
          bg: `var(--bg-color)`,
          ".navbar-inverted &": {
            bg: `transparent`,
          },
          position: [null, `absolute`],
          flexDirection: `column`,
          pt: [`15px`, `10px`],
          pb: [0, `10px`],
          mx: [`-15px`],
          px: [`15px`],
          alignItems: [`center`, `start`],
        }}
      >
        {items.map(({ title: itemTitle, to }) => (
          <NavLink
            key={itemTitle}
            to={to}
            customSx={{
              mb: `10px`,
              fontSize: [`23px`, `16px`, `18px`],
            }}
            customClassName={navLinkClassName}
          >
            {itemTitle}
          </NavLink>
        ))}
      </Flex>
    </Box>
  )
}

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
        fontSize: menuItemFontSize,
        lineHeight: [1.8, 1.1],
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
      display: [`block`, `none`],
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
  const [isMobileNavbarOpened, setIsMobileNavbarOpened] = useState(false)
  const toggleNav = () => setIsMobileNavbarOpened(p => !p)

  return (
    <Flex
      sx={{
        flexWrap: `wrap`,
        p: [`15px`, `25px`, `35px`],
        justifyContent: [`space-between`],
        alignItems: `center`,
        position: `fixed`,
        top: 0,
        right: 0,
        left: 0,
        zIndex: 999,
        mt: [0, `-16px`, fullpage ? `-14px` : null],
        transition: `all 0.4s ease`,
      }}
      className={`transition-section header ${inverted && `navbar-inverted`} ${
        isMobileNavbarOpened && `in-viewport menu-active`
      }`}
    >
      <Box
        sx={{
          width: [`175px`, `275px`, `300px`],
          mt: [0, `16px`, fullpage ? 0 : null],
          pb: [0, `7px`],
          transform: [null, null, fullpage ? `scale(0.9)` : null],
        }}
      >
        <Logo />
      </Box>
      <Flex
        className={`transition-section__transition--slide-fade-in top-in duration-3 delayed-0 min-0--none`}
        sx={{
          flexDirection: [`column`, `row`],
          flexBasis: [`100%`, `auto`],
          ml: [null, null, `auto`],
          mt: [0, `16px`, fullpage ? 0 : null],
          alignItems: [`center`, `flex-end`],
          height: [`100vh`, `auto`],
          opacity: [navbarState ? 1 : 0, 1],
          zIndex: [navbarState ? 99 : -999, 99],
          position: [`absolute`, `unset`],
          top: [`45px`, 0],
          right: [0],
          left: [0],
          pt: [`66px`, 0],
          background: [`var(--bg-color)`, `inherit`],
        }}
      >
        <NavLink
          to="/services"
          customSx={{
            mx: [0, `15px`, `25px`],
            mr: [0, `15px`, `25px`],
            ml: [0],
            mb: [`15px`, 0],
          }}
          customClassName={navLinkClassName}
        >
          Services
        </NavLink>
        <Dropdown
          title="Key industries"
          items={[
            { title: `Biotech`, to: `/industry/biotech` },
            { title: `Fintech`, to: `/industry/fintech` },
            { title: `Autonomous Vehicles`, to: `/industry/autonomous` },
          ]}
        />
        <Dropdown
          title="Team"
          items={[
            { title: `Members`, to: `/team` },
            { title: `Groups`, to: `/groups` },
          ]}
        />
        {[
          { title: `Open source`, to: `/opensource` },
          { title: `Contact`, to: `/contact` },
          {
            title: `Careers`,
            to: `//boards.greenhouse.io/tweag`,
            isExternal: true,
          },
          { title: `Research`, to: `/research` },
          { title: `Blog`, to: `/blog` },
        ].map(({ title, to, isExternal }, i, arr) => (
          <NavLink
            customClassName={navLinkClassName}
            key={title}
            to={to}
            isExternal={isExternal}
            customSx={{
              mx: [0, `15px`, `25px`],
              ...(i === arr.length - 1 ? { mr: [0] } : {}),
              minWidth: [`fit-content`],
              mb: [`15px`, 0],
            }}
          >
            {title}
          </NavLink>
        ))}
      </Flex>
      <MobileMenuOpener onClick={toggleNav} />
    </Flex>
  )
}

export default Header

/** @jsx jsx */
import { jsx } from "theme-ui"

const NavItem = props => (
  <li
    {...props}
    sx={{
      a: {
        position: `relative`,
        color: `var(--fg-color)`,
        overflow: `hidden`,
        zIndex: 2,
      },
      "a::after": {
        content: `""`,
        position: `absolute`,
        left: 0,
        width: 0,
        top: `22px`,
        height: `2px`,
        bottom: 0,
        transition: `all 0.4s ease`,
        background: `var(--fg-color)`,
      },
      "a:hover::after": {
        width: `100%`,
      },
    }}
  />
)

export default NavItem

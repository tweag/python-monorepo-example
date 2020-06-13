/** @jsx jsx */
import { jsx } from "theme-ui"

const NavItem = props => (
  <li
    {...props}
    sx={{
      a: {
        position: `relative`,
        color: `black`,
        overflow: `hidden`,
        zIndex: 2,
      },
      "a::after": {
        content: `""`,
        position: `absolute`,
        left: 0,
        width: 0,
        top: `20px`,
        height: `2px`,
        bottom: 0,
        transition: `all 0.4s ease`,
        background: `black`,
      },
      "a:hover::after": {
        width: `100%`,
      },
    }}
  />
)

export default NavItem

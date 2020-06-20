/** @jsx jsx */
import { jsx } from "theme-ui"

const NavList = props => (
  <ul
    {...props}
    sx={{
      m: 0,
      p: 0,
      listStyle: `none`,
      position: `relative`,
    }}
  />
)

export default NavList

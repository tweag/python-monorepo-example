/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link } from "gatsby"

import NavList from "./navlist"
import NavItem from "./navitem"

import logo from "../images/tweag_logo_footer.svg"

const ContactUs = () => (
  <Link
    to="/contact"
    sx={{
      fontSize: 1,
      p: `5px 18px`,
      height: `auto`,
    }}
    className="btn"
  >
    Contact us
  </Link>
)

const ColumnArea = props => (
  <div
    {...props}
    sx={{
      display: `flex`,
      flexWrap: `wrap`,
      justifyContent: `space-between`,
      alignItems: `flex-start`,
    }}
  />
)

const Column = props => (
  <div
    {...props}
    sx={{
      position: `relative`,
      width: [`50%`, `15%`],
      m: [`0 0 20px`, `0 0 22px`],
      p: [`0 10px`, `0`],
    }}
  />
)

const ColumnTitle = props => (
  <strong
    {...props}
    sx={{
      fontSize: 3,
      lineHeight: `24px`,
      fontWeight: `400`,
      display: `block`,
      m: [`0 0 10px`, `0 0 20px`],
    }}
  />
)

const Footer = () => (
  <footer
    sx={{
      fontSize: [0, 1, null, null, null, 2],
      lineHeight: [`18px`, `24px`, null, null, null, `26px`],
      position: `relative`,
      bg: `black`,
      color: `white`,
      "--bg-color": `black`,
      "--fg-color": `white`,
      p: [`40px 0`, `80px 50px 0`, null, null, null, `60px 120px`],
      width: `100%`,
    }}
  >
    <ColumnArea>
      <Column>
        <div className="mail-area">
          <strong className="mail-title">
            Interested in working together?
          </strong>
          <ContactUs />
        </div>
      </Column>
      <Column>
        <ColumnTitle>See our work</ColumnTitle>
        <NavList>
          <NavItem>
            <Link to="/industry/biotech">Biotech</Link>
          </NavItem>
          <NavItem>
            <Link to="/industry/fintech">Fintech</Link>
          </NavItem>
          <NavItem>
            <Link to="/industry/autonomous">Autonomous vehicles</Link>
          </NavItem>
          <NavItem>
            <Link to="/opensource">Open source</Link>
          </NavItem>
        </NavList>
      </Column>
      <Column>
        <ColumnTitle>Stay connected</ColumnTitle>
        <NavList>
          <NavItem>
            <a href="https://twitter.com/tweagio">Twitter</a>
          </NavItem>
          <NavItem>
            <a href="https://github.com/tweag">GitHub</a>
          </NavItem>
          <NavItem>
            <a href="https://www.linkedin.com/company/tweag-i-o/">Linkedin</a>
          </NavItem>
          <NavItem>
            <a href="/rss.xml">RSS</a>
          </NavItem>
        </NavList>
      </Column>
      <Column>
        <ColumnTitle>About us</ColumnTitle>
        <NavList>
          <NavItem>
            <Link to="/services">Services</Link>
          </NavItem>
          <NavItem>
            <Link to="/blog">Blog</Link>
          </NavItem>
          <NavItem>
            <Link to="/contact">Careers</Link>
          </NavItem>
        </NavList>
      </Column>
      <Column sx={{ paddingTop: `20px` }}>
        <strong className="f-logo">
          <Link to="/">
            <img src={logo} alt="Tweag" />
          </Link>
        </strong>
      </Column>
    </ColumnArea>
    <div className="copyright-area">
      <div className="copyright-col1">
        <span className="copyright">
          Tweag HQ &rarr; 207 Rue de Bercy &mdash; 75012 Paris &mdash; France
          <span>
            <br />
            <span>hello@tweag.io</span>
          </span>
        </span>
      </div>
      <div className="copyright-col2">
        <span className="copyright">
          &copy; 2020 Tweag.&nbsp;All rights reserved
          <span>
            <br />
            <span>Privacy Policy</span>
          </span>
        </span>
      </div>
    </div>
  </footer>
)

export default Footer

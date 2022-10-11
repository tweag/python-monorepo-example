/** @jsx jsx */
import { jsx } from "theme-ui"
import { Link } from "gatsby"

import NavList from "./navlist"
import NavItem from "./navitem"

import logo from "../images/tweag_logo_footer.svg"

const ContactUs = () => (
  <Link
    to="/careers"
    className="button button-secondary button-medium inverted pre-arrow-right"
  >
    Join us
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
      width: [`50%`, `50%`, `15%`],
      m: [`0 0 20px`, `0 0 20px`, `0 0 22px`],
      p: [`0 10px`, `0 10px`, `0`],
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
      m: [`0 0 10px`, `0 0 10px`, `0 0 20px`],
    }}
  />
)

const Footer = () => (
  <footer
    sx={{
      fontSize: [0, 0, 1, null, null, null, 2],
      lineHeight: [`18px`, `18px`, `24px`, null, null, null, `26px`],
      position: `relative`,
      bg: `black`,
      color: `white`,
      "--bg-color": `black`,
      "--fg-color": `white`,
      p: [`40px 0`, `40px 0`, `80px 50px 0`, null, null, null, `60px 120px`],
      width: `100%`,
    }}
  >
    <ColumnArea
      sx={{
        marginBottom: `40px`,
      }}
    >
      <Column>
        <div
          sx={{
            display: `block`,
            position: `relative`,
          }}
        >
          <strong
            sx={{
              fontWeight: [300],
              position: `relative`,
              display: `block`,
              margin: [`0px 0px 20px`],
              fontSize: [3],
              lineHeight: [`24px`],
            }}
          >
            Interested in working at Tweag?
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
            <a href="https://www.linkedin.com/company/tweag-i-o/">LinkedIn</a>
          </NavItem>
          <NavItem>
            <a href="https://www.compositional.fm/">Podcast</a>
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
            <Link to="/careers">Careers</Link>
          </NavItem>
          <NavItem>
            <Link to="/contact">Contact us</Link>
          </NavItem>
        </NavList>
      </Column>
      <Column sx={{ paddingTop: `20px` }}>
        <strong
          sx={{
            position: `relative`,
            display: `block`,
            width: [`120px`, `120px`, `152px`],
          }}
        >
          <Link to="/" style={{ display: `block` }}>
            <img
              sx={{
                display: `block`,
                width: `100%`,
                height: `auto`,
              }}
              src={logo}
              alt="Tweag"
            />
          </Link>
        </strong>
      </Column>
    </ColumnArea>
    <div
      sx={{
        fontSize: [0],
        position: `relative`,
        display: [`block`, `block`, `flex`],
        flexWrap: `wrap`,
        alignItems: `center`,
        justifyContent: `space-between`,
        margin: [
          `0px -15px 25px`,
          `0px -15px 25px`,
          0,
          0,
          0,
          0,
          `50px 0px 25px`,
        ],
        padding: [`25px`, `25px`, 0],
      }}
    >
      <div
        sx={{
          margin: `0 0 22px`,
          position: `relative`,
          width: [`100%`, `100%`, `60%`],
        }}
      >
        <span
          sx={{
            position: `relative`,
            display: `block`,
            margin: [`0 0 20px`, `0 0 20px`, 0],
          }}
        >
          Tweag HQ &rarr; 207 Rue de Bercy &mdash; 75012 Paris &mdash; France
          <span>
            <br />
            <span>hello@tweag.io</span>
          </span>
        </span>
      </div>
      <div
        sx={{
          margin: `0 0 22px`,
          position: `relative`,
          width: [`100%`, `100%`, `40%`],
        }}
      >
        <span
          sx={{
            position: `relative`,
            display: [`block`, `block`, `inline-block`],
            float: [`none`, `none`, `right`],
            margin: [`0 0 20px`, `0 0 20px`, 0],
          }}
        >
          &copy; Tweag I/O Limited.&nbsp;{` `}
          <a
            href="https://moduscreate.com"
            sx={{
              color: `#f28a00`,
            }}
          >
            A Modus Create Company
          </a>
          .
          <span>
            <br />
            <span>All rights reserved</span>
            <br />
            <Link to="/privacy-policy">
              <div sx={{color: `#ffffff`}}>Privacy Policy</div>
            </Link>
          </span>
        </span>
      </div>
    </div>
  </footer>
)

export default Footer

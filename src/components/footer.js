import React from "react"
import { Link } from "gatsby"

import "./footer.css"
import logo from "../images/tweag_logo_footer.svg"

const Footer = () => {
  return (
    <footer className="footer">
      <div className="container">
        <div className="footer-col-area">
          <div className="footer-col">
            <div className="mail-area">
              <strong className="mail-title">
                Interested in working together?
              </strong>
              <Link to="/contact" className="btn">
                Contact us
              </Link>
            </div>
          </div>
          <div className="footer-col">
            <strong className="title">See our work</strong>
            <ul className="footer-nav">
              <li>
                <Link to="/industry/biotech">Biotech</Link>
              </li>
              <li>
                <Link to="/industry/fintech">Fintech</Link>
              </li>
              <li>
                <Link to="/industry/autonomous">Autonomous vehicles</Link>
              </li>
              <li>
                <Link to="/opensource">Open source</Link>
              </li>
            </ul>
          </div>
          <div className="footer-col">
            <strong className="title">Stay connected</strong>
            <ul className="footer-nav">
              <li>
                <a href="https://twitter.com/tweagio">Twitter</a>
              </li>
              <li>
                <a href="https://github.com/tweag">GitHub</a>
              </li>
              <li>
                <a href="https://www.linkedin.com/company/tweag-i-o/">
                  Linkedin
                </a>
              </li>
              <li>
                <a href="/rss.xml">RSS</a>
              </li>
            </ul>
          </div>
          <div className="footer-col">
            <strong className="title">About us</strong>
            <ul className="footer-nav">
              <li>
                <Link to="/services">Services</Link>
              </li>
              <li>
                <Link to="/blog">Blog</Link>
              </li>
            </ul>
          </div>
          <div className="footer-col tweag">
            <strong className="f-logo">
              <Link to="/">
                <img src={logo} alt="Tweag" />
              </Link>
            </strong>
          </div>
        </div>
        <div className="copyright-area">
          <div className="copyright-col1">
            <span className="copyright">
              Tweag HQ &rarr; 207 Rue de Bercy &mdash; 75012 Paris &mdash;
              France
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
      </div>
    </footer>
  )
}

export default Footer

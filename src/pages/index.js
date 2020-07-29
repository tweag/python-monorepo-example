import React from "react"
import { Link } from "gatsby"
import Typed from "react-typed"

import Footer from "../components/footer"
import KeyIndustries from "../components/keyindustries"
import LayoutFullPage from "../components/layout-fullpage"
import SEO from "../components/seo"

import homePreview from "../images/home_preview.png"
import homeVideo from "../images/home.mp4"
import visionaries from "../images/img2.gif"
import logoAmgen from "../images/logo_amgen.png"
import logoCea from "../images/logo_cea.png"
import logoGoogle from "../images/logo_google.png"
import logoPfizer from "../images/logo_pfizer.png"
import logoTarget from "../images/logo_target.png"
import logoOrange from "../images/logo_orange.png"

const IndexPage = ({ data, location }) => {
  return (
    <LayoutFullPage>
      <SEO title="Software innovation lab" />
      <div id="fullpage">
        <div className="section section01">
          <div className="visual-area section-wrap head1">
            <div className="container fullp animation-wrap">
              <div className="visual-holder">
                <div className="image-holder">
                  <video
                    width="600"
                    height="600"
                    loop
                    autoPlay
                    muted
                    playsInline
                    data-keepplaying
                    poster={homePreview}
                  >
                    <source src={homeVideo} type="video/mp4" />
                  </video>
                </div>
                <div className="visual-caption">
                  <h1 className="typeit">
                    <Typed
                      strings={[`SCALE YOUR ENGINEERING POWER.`]}
                      typeSpeed={50}
                      showCursor={false}
                    />
                  </h1>
                  <p>
                    We enable deep tech startups to achieve their vision, from
                    research to product delivery.
                  </p>
                </div>
              </div>
            </div>
          </div>
          <div className="line-arrow down"></div>
        </div>
        <div className="section section02">
          <div className="contactus-area section-wrap">
            <div className="container fullp">
              <div className="contactus-holder animation-wrap">
                <div className="image-col">
                  <h2>PARTNERING WITH VISIONARIES TO ADVANCE TECHNOLOGY</h2>

                  <div className="image-holder">
                    <img src={visionaries} alt="Partnering with visionaries" />
                  </div>
                </div>
                <div className="text-area">
                  <ul className="text-list">
                    <li>
                      <h3>Scale with confidence</h3>
                      <p>
                        Quickly grow your team with vetted, senior engineers.
                        Tweag provides the expertise needed to execute
                        high-risk, high-reward projects.
                      </p>
                    </li>
                    <li>
                      <h3>Increase developer productivity</h3>
                      <p>
                        Gain a developer efficiency boost through software best
                        practices and processes that accelerate your time to
                        market.
                      </p>
                    </li>
                    <li>
                      <h3>Solve complex problems</h3>
                      <p>
                        Realize your breakthrough vision. Together, we&apos;ll
                        iterate quickly and transform your ideas into products
                        that work.
                      </p>
                    </li>
                  </ul>
                  <Link to="/contact" className="btn">
                    Contact us
                  </Link>
                </div>
              </div>
            </div>
          </div>
        </div>
        <div className="section section03">
          <KeyIndustries isFullPage={true} />
        </div>
        <div className="section section04">
          <div className="joinus-area section-wrap">
            <div className="container">
              <div className="joinus-holder animation-wrap">
                <div className="text-wrap">
                  <p className="title">
                    At Tweag, we drive purposeful innovation through lasting
                    software. We apply mathematics, computer science and open
                    source methodologies to advance software engineering.
                  </p>
                </div>
                <div
                  className="image-holder backdrop"
                  style={{ animation: `none` }}
                ></div>
              </div>
            </div>
          </div>
        </div>
        <div className="section section05">
          <div className="partners-area section-wrap">
            <div className="container animation-wrap">
              <h2>
                TRUSTED BY LEADERS <br />
                &amp; INNOVATORS
              </h2>
              <ul className="partners-list">
                <li>
                  <div className="logo-wrap">
                    <img src={logoAmgen} width="200" alt="Amgen" />
                  </div>
                </li>
                <li>
                  <div className="logo-wrap">
                    <img src={logoCea} width="100" alt="CEA" />
                  </div>
                </li>
                <li>
                  <div className="logo-wrap">
                    <img src={logoGoogle} width="200" alt="Google" />
                  </div>
                </li>
                <li>
                  <div className="logo-wrap">
                    <img src={logoOrange} width="100" alt="Orange" />
                  </div>
                </li>
                <li>
                  <div className="logo-wrap">
                    <img src={logoPfizer} width="150" alt="Pfizer" />
                  </div>
                </li>
                <li>
                  <div className="logo-wrap">
                    <img src={logoTarget} width="100" alt="Target" />
                  </div>
                </li>
              </ul>
            </div>
          </div>
        </div>
        <div className="section section06">
          <div className="vision-area home-vision animation-wrap">
            <div className="container">
              <div className="vision-holder">
                <div className="text-col">
                  <h2>READY TO ACHIEVE YOUR BIG VISION?</h2>
                  <Link to="/contact" className="btn">
                    Contact us
                  </Link>
                </div>
                <div className="image-holder backdrop2"></div>
              </div>
            </div>
          </div>
          <Footer />
        </div>
      </div>
    </LayoutFullPage>
  )
}

export default IndexPage

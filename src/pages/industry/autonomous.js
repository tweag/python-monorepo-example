import React from "react"
import { Link } from "gatsby"

import CallToAction from "../../components/cta"
import Layout from "../../components/layout"
import SEO from "../../components/seo"

import img9 from "../../images/img9.svg"
import img10 from "../../images/img10.svg"
import img11 from "../../images/img11.svg"

const AutonomousPage = () => {
  return (
    <Layout>
      <SEO title="Autonomous vehicles" />
      <section className="section-area">
        <div className="section s_white services-section biotech viewport-section">
          <div className="text-area">
            <div className="section-title">Key industry</div>
            <h3>
              <i className="icon-arrow-right1"></i> Autonomous vehicles
            </h3>
            <h1>
              WE PARTNER WITH VISIONARY STARTUPS AND GLOBAL LEADERS TO BUILD
              SELF-DRIVING CARS.
            </h1>
            <div className="text-wrap">
              <p>
                Automobile safety is critical. There is no room for bugs when
                human life is at stake. Tweag builds high-assurance software
                that meets the extensive safety demands of driverless
                technology. We help clients design and deploy solutions that are
                reliable, reproducible and maintainable. Our clients are shaping
                the future of driverless technology.
              </p>
            </div>
          </div>
        </div>
        <div className="section s_white services-section biotech viewport-section">
          <div className="image-holder image-holder2">
            <img src={img9} alt="" />
          </div>
          <div className="text-wrap text-area2">
            <p>
              Our approach to safety comes down to science. We apply lightweight
              formal methods to create testing and analysis tools that find bugs
              early.
            </p>
            <p style={{ marginTop: "1.5rem" }}>
              Our capabilities go beyond safety. Tweag helps you quickly scale
              your engineering performance to build and innovate faster. Our
              expertise in infrastructure and software development processes
              enable you to resolve bottlenecks and increase developer
              productivity and output. Safer and faster development is not a
              choice you have to make.
            </p>
          </div>
        </div>
        <div className="section s_green about-section biotech learnmore viewport-section">
          <div className="image-holder image-holder4">
            <img src={img11} alt="" />
          </div>
          <div className="text-area">
            <div className="section-title">Case studies</div>
            <h1>Learn more about our projects.</h1>
            <div className="split">
              <div className="split-col1">
                <h3>Kitty Hawk</h3>
                <p>
                  Generating embedded code for the autopilot of flying
                  autonomous taxis.
                </p>
                <p>
                  Building high-performance, auditable and traceable continous
                  integration.
                </p>
              </div>
              <div className="split-col2">
                <h3>La Fabrique des Mobilités</h3>
                <p>
                  End-to-end solution to record accident parameters on the
                  blockchain and improve driving behaviour.
                </p>
                <p>
                  Won{" "}
                  <a href="http://occ-challenge.mystrikingly.com/">2nd prize</a>{" "}
                  of the Open Connected Cars Challenge.
                </p>
              </div>
              <h3 className="quote">
                “Tweag all very smart people. Everyone is really energetic and
                intelligent and competent. They’ve made really good decisions. I
                trust them.”
              </h3>
              <span className="text">
                <i className="icon-arrow-right1"></i>&nbsp;&nbsp;Greg Horn,
                Director of Software and Controls, Kitty Hawk
              </span>
            </div>
          </div>
        </div>
        <div className="about-section viewport-section"></div>
        <div className="section s_beige section06a viewport-section">
          <CallToAction areaIndex={3} backdropIndex={4} />
        </div>
      </section>
    </Layout>
  )
}

export default AutonomousPage

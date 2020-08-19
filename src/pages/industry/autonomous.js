import React from "react"

import CallToAction from "../../components/cta"
import Layout from "../../components/layout"
import SEO from "../../components/seo"

import img9 from "../../images/img9.svg"

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
              We partner with visionary startups and global leaders to build
              self-driving cars
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
            <p style={{ marginTop: `1.5rem` }}>
              Our capabilities go beyond safety. Tweag helps you quickly scale
              your engineering performance to build and innovate faster. Our
              expertise in infrastructure and software development processes
              enable you to resolve bottlenecks and increase developer
              productivity and output. Safer and faster development is not a
              choice you have to make.
            </p>
          </div>
        </div>
        <div className="about-section viewport-section"></div>
        <div className="section s_beige viewport-section">
          <CallToAction backdropIndex={4} />
        </div>
      </section>
    </Layout>
  )
}

export default AutonomousPage

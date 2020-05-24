import React from "react"
import { Link } from "gatsby"

import Layout from "../../components/layout"
import SEO from "../../components/seo"

import img9 from "../../images/img9.svg"
import img10 from "../../images/img10.svg"
import img11 from "../../images/img11.svg"

const BiotechPage = () => {
  return (
    <Layout>
      <SEO title="Biotech" />
      <section className="section-area">
        <div className="services-section biotech viewport-section">
          <div className="text-area">
            <div className="section-title">Key industry</div>
            <h3>
              <i className="icon-arrow-right1"></i> BIOTECHNOLOGY
            </h3>
            <h1>
              WE PARTNER WITH VISIONARY STARTUPS AND GLOBAL LEADERS TO ADVANCE
              MEDICINE THROUGH TECHNOLOGY.
            </h1>
            <div className="text-wrap">
              <p>
                Biotechnology deals with enormous amounts of heterogeneous data
                from a range of different sources. Tweag enables biotechnology
                companies to organize and streamline this diverse data to train
                powerful statistical models that guide meaningful discoveries
                and business decisions.
              </p>
            </div>
          </div>
        </div>
        <div className="services-section biotech viewport-section">
          <div className="image-holder image-holder2">
            <img src={img9} alt="" />
          </div>
          <div className="text-wrap text-area2">
            <p>
              We combine software engineering with life sciences expertise
              &ndash; Tweag engineers have academic and industry backgrounds in
              human genetics, biochemistry and related fields. This gives us a
              deeper understanding of your vision and the ability to implement
              the most effective solution.
            </p>
          </div>
        </div>
        <div className="about-section biotech viewport-section">
          <div className="text-area">
            <h3> Examples of Tweag&rsquo;s work in Biotechnology includes:</h3>
            <ul className="text-list">
              <li>
                <div className="icon-arrow-right1"> </div>
                <p>
                  Maintaining a database and web portal that gathers and
                  presents genotype-phenotype data for <strong>Pfizer.</strong>
                </p>
              </li>
              <li>
                <div className="icon-arrow-right1"></div>
                <p>
                  Developing software that runs models of blood flow in the
                  human body to analyze drug effects on patients with{" "}
                  <strong>NovaDiscovery.</strong>
                </p>
              </li>
              <li>
                <div className="icon-arrow-right1"></div>
                <p>
                  Creating data pipelines and a data lake that allows
                  bioanalysts to access a large variety of genetic data in
                  natural form for <strong>Pfizer.</strong>
                </p>
              </li>
              <li>
                <div className="icon-arrow-right1"></div>
                <p>
                  Developing off-the-shelf statistical routines and analysis
                  tools to guarantee the safety and correctness of software
                  medical devices with <strong>Amgen.</strong>
                </p>
              </li>
            </ul>
            <Link to="/contact" className="contact btn">
              Contact us
            </Link>
          </div>
          <div className="image-holder image-holder3">
            <img src={img10} alt="" />
          </div>
        </div>
        <div className="about-section biotech learnmore viewport-section">
          <div className="image-holder image-holder4">
            <img src={img11} alt="" />
          </div>
          <div className="text-area">
            <div className="section-title">Case studies</div>
            <h1> learn more about our projects.</h1>
            <div className="split">
              <div className="split-col1">
                <h3>Pfizer</h3>
                <p>Making genetic data accessible for drug discovery.</p>
                <Link to="/case-study/pfizer" className="btn">
                  Read case study
                </Link>
              </div>
              <div className="split-col2">
                <h3>NovaDiscovery</h3>
                <p>Simulating human physiology to cut clinical trial costs.</p>
                <Link to="/case-study/novadiscovery" className="btn">
                  Read case study
                </Link>
              </div>
              <h3 className="quote">
                I really value Tweag&rsquo;s rapid prototyping and their
                creativity and willingness to try different things.
              </h3>
              <span className="text">
                <i className="icon-arrow-right1"></i>&nbsp;&nbsp;Eric Fauma,
                Senior Scientific Director, Head Integrative Biology, Pfizer
              </span>
            </div>
          </div>
        </div>
        <div className="about-section viewport-section"></div>
      </section>
    </Layout>
  )
}

export default BiotechPage

import React from "react"
import { Link } from "gatsby"

import CallToAction from "../../components/cta"
import Layout from "../../components/layout"
import SEO from "../../components/seo"

import img9 from "../../images/img9.svg"
import img10 from "../../images/img10.svg"
import img11 from "../../images/img11.svg"

const FintechPage = () => {
  return (
    <Layout>
      <SEO title="Fintech" />
      <section className="section-area">
        <div className="section s_white services-section biotech viewport-section">
          <div className="text-area">
            <div className="section-title">Key industry</div>
            <h3>
              <i className="icon-arrow-right1"></i> FINTECH
            </h3>
            <h1>
              WE PARTNER WITH VISIONARY STARTUPS AND GLOBAL LEADERS TO MANAGE
              ASSETS SAFELY.
            </h1>
            <div className="text-wrap">
              <p>
                Financial assets carry plenty of risk already. The financial
                industry has no appetite for adding risk via the systems that
                are used to manage financial assets. The industry requires tools
                and systems worthy of trust.
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
              Tweag specializes in building high-assurance software for fintech
              companies, from pioneering blockchain startups to large global
              banks. We apply lightweight formal methods that ensure key system
              components are built to specification. We use modern software
              engineering practices that make it easier to find and eliminate
              bugs in a transparent way.
            </p>
            <p style={{ marginTop: `1.5rem` }}>
              We also understand financial products. Our background in finance,
              mathematics and data science means we speak your language, and we
              can reach a deep understanding of your challenges and goals. This
              enables us to build innovative tools that make your professional
              team be more efficient and productive in their work.
            </p>
          </div>
        </div>
        <div className="section s_beige about-section biotech viewport-section">
          <div className="text-area">
            <h3> Examples of Tweag&rsquo;s work in fintech includes:</h3>
            <ul className="text-list">
              <li>
                <div className="icon-arrow-right1"> </div>
                <p>
                  Tweagers were leaders of the design of Plutus, the smart
                  contract language for one of the world&rsquo;s top-10
                  cryptocurrencies with <strong>IOHK</strong>
                  {`.`}
                </p>
              </li>
              <li>
                <div className="icon-arrow-right1"></div>
                <p>
                  We created a new booking system for equity swaps for a global
                  bank.
                </p>
              </li>
              <li>
                <div className="icon-arrow-right1"></div>
                <p>
                  Tweag streamlined devops and infrastructure for{` `}
                  <strong>Digital Asset</strong> so they could focus on things
                  like replacing the ASX legacy clearing and settlement system
                  with multi-party, automated, and simplified workflows.
                </p>
              </li>
              <li>
                <div className="icon-arrow-right1"></div>
                <p>
                  We helped <strong>Gain Theory</strong> rethink marketing
                  investments with ROI models powered by software built by
                  Tweag.
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
        <div className="section s_green about-section biotech learnmore viewport-section">
          <div className="image-holder image-holder4">
            <img src={img11} alt="" />
          </div>
          <div className="text-area">
            <div className="section-title">Case studies</div>
            <h1>Learn more about our projects.</h1>
            <div className="split">
              <div className="split-col1">
                <h3>IOHK</h3>
                <p>
                  Designing Plutus, the smart contract language for a world
                  top-10 currency.
                </p>
              </div>
              <div className="split-col2"></div>
              <h3 className="quote">
                Working with Tweag has been a great honor. Their team offers
                exceptional talent and a technical skill set that we might not
                have otherwise found in our direct recruitment efforts. They
                provide our executive team with insightful guidance, and always
                makes us feel like they are part of our team.
              </h3>
              <span className="text">
                <i className="icon-arrow-right1"></i>&nbsp;&nbsp;Tamara Haasen,
                Chief of Staff, IOHK
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

export default FintechPage

import React from "react"

import KeyIndustries from "../components/keyindustries"
import Layout from "../components/layout"
import SEO from "../components/seo"

import img6 from "../images/img6.svg"
import img7 from "../images/img7.svg"
import img8 from "../images/img8.svg"

const Services = () => {
  return (
    <div className="services-section viewport-section">
      <div className="text-area">
        <a className="section-title" href="#services" id="services">
          Services
        </a>
        <h1>
          <i></i> TWEAG TRANSLATES AMBITIOUS VISIONS INTO TECHNOLOGY SOLUTIONS
          THAT WORK.
        </h1>
        <div className="text-wrap">
          <p>
            We combine solid engineering principles with new ideas from academia
            to solve complex problems and build critical systems.
          </p>
          <p>
            We take on your project as our own. Our engineers embed into your
            team, injecting experience and technical expertise from
            proof-of-concept to product delivery. Achieve your vision with
            confidence by partnering with Tweag.
          </p>
        </div>
      </div>
      <div className="image-holder hideresp">
        <img src={img6} alt="" />
      </div>
    </div>
  )
}

const Quote = () => {
  return (
    <div className="method-area viewport-section">
      <div className="image-holder">
        <img src={img7} alt="" />
      </div>
      <div className="text-area">
        <h2>
          It&rsquo;s really, really great working with Tweag. The quality of
          their work is top-notch. Their productivity is higher than average.
          And they&rsquo;re domain experts in languages.
        </h2>
        <span className="text">
          <i className="icon-arrow-right1"></i> Fred Cogny, CTO, NovaDiscovery
        </span>
      </div>
    </div>
  )
}

const WhatWeDo = () => {
  return (
    <div className="about-section viewport-section">
      <div className="text-area">
        <a className="section-title" href="#what-we-do" id="what-we-do">
          What we do
        </a>
        <ul className="text-list">
          <li>
            <i className="icon-arrow-right1"></i> DevOps &amp; Infrastructure
            <p>
              Gain a developer efficiency boost through software best practices
              and processes that accelerate your time to market.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Statistics &amp; Machine
            Learning
            <p>
              Gain a developer efficiency boost through software best practices
              and processes that accelerate your time to market.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Software Engineering
            <p>
              Gain a developer efficiency boost through software best practices
              and processes that accelerate your time to market.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Applied Research
            <p>
              Gain a developer efficiency boost through software best practices
              and processes that accelerate your time to market.
            </p>
          </li>
        </ul>
      </div>
      <div className="image-holder">
        <img src={img8} alt="" />
      </div>
    </div>
  )
}

const ServicesPage = () => {
  return (
    <Layout>
      <SEO title="Our services" />
      <div className="section s_white section-area services">
        <Services />
        <Quote />
        <WhatWeDo />
      </div>
      <div className="line_sep"></div>
      <KeyIndustries />
    </Layout>
  )
}

export default ServicesPage

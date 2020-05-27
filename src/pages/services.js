import React from "react"

import CallToAction from "../components/cta"
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
          And they&rsquo;re domain experts in programming languages.
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
            <i className="icon-arrow-right1"></i> Software Engineering
            <p>
              We use tools and programming languages that represent the future
              of reliable software development. Reliability is baked into how we
              build your software.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> DevOps &amp; Infrastructure
            <p>
              Software gets rebuilt and deployed over and over again. We
              specialize in making that process efficient for your business and
              auditable end-to-end so you always know exactly what is in
              production. It&rsquo;s one of our favorite things to do.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Statistics &amp; Machine
            Learning
            <p>
              We know how to train and deploy models. We are also here to
              address the growing pains that organizations face in building out
              this new capability.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Applied Research
            <p>
              There’s nothing more practical than applying the right theory. We
              have a track record in taking the best ideas from research and
              successfully bringing them to market.
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
      <div className="section s_orange section06a viewport-section">
        <CallToAction areaIndex={2} backdropIndex={3} />
      </div>
    </Layout>
  )
}

export default ServicesPage

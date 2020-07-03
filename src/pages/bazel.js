import React from "react"

import Layout from "../components/layout"
import CallToAction from "../components/cta"

import img12 from "../images/img12.svg"

const Manifesto = () => {
  return (
    <>
      <div className="section s_white services-section opensource1 viewport-section">
        <div className="text-area">
          <a className="section-title" href="#bazel" id="bazel">
            Bazel
          </a>
          <h1>OPTIMIZE YOUR BAZEL IMPLEMENTATION.</h1>
          <div className="text-wrap">
            <p>
              Tweag is the software innovation lab driving Bazel’s worldwide
              adoption and ongoing development. As Google’s first Bazel
              Community Expert, we can help you optimize every phase of your
              Bazel implementation, from preparing for migration to improving
              your existing setup.
            </p>
            <p>
              We can answer your toughest Bazel questions. Combining rich Bazel
              skills with deep technology expertise, Tweag offers rare insight
              into Bazel&rsquo;s capabilities (and challenges) across diverse
              environments.
            </p>
          </div>
        </div>
        <div className="image-holder image-holder hideresp">
          <img src={img12} alt="" />
        </div>
      </div>
    </>
  )
}

const Community = ({ title, className, children }) => {
  return (
    <div
      className={`section about-section opensource ${className} viewport-section`}
    >
      <div className="text-wrap text-area w30">
        <h1>{title}</h1>
      </div>
      <div className="text-wrap text-area w70">{children}</div>
    </div>
  )
}

const BazelPage = () => {
  return (
    <Layout>
      <section className="section-area">
        <Manifesto />
        <Community title="Ready" className="s_red opensource">
          <p>
            Get a head start on your Bazel migration with Tweag’s Readiness
            Assessment. We&rsquo;ll document expected improvements, potential
            challenges and required resources to help you plan for a successful
            migration to a Bazel build system.
          </p>
        </Community>
        <Community title="Implement" className="s_orange opensource3">
          <p>
            Harness Bazel&rsquo;s power after migration. We&rsquo;ll help you
            tune your Bazel to enjoy dramatic improvements in productivity and
            performance. From improving cache hits to writing custom rules, we
            have experience adapting Bazel to your needs. We&rsquo;ve also been
            successful advocates for getting solutions to our client&rsquo;s
            use-case requirements incorporated into core Bazel.
          </p>
        </Community>
        <Community title="Result" className="s_grey opensource2">
          <p>We have worked with clients across a range of industries:</p>
          <p className="tab">
            <i className="icon-arrow-right1">
              {` `}
              In silico simulations of human physiology
            </i>
            <br />
            <i className="icon-arrow-right1">
              {` `}
              Novel static analysis tools for the autopilot software in
              autonomous taxis
            </i>
            <br />
            <i className="icon-arrow-right1">
              {` `}
              Designing smart contract languages
            </i>
            <br />
            <i className="icon-arrow-right1">
              {` `}
              Improving the performance of third generation blockchains
            </i>
          </p>
        </Community>
        <div className="section s_white viewport-section">
          <CallToAction areaIndex={4} backdropIndex={3} />
        </div>
      </section>
    </Layout>
  )
}

export default BazelPage

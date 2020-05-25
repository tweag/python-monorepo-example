import React from "react"
import { Link } from "gatsby"
import Layout from "../components/layout"
import SEO from "../components/seo"

import img12 from "../images/img12.svg"
import img13 from "../images/img13.svg"
import img14 from "../images/img14.svg"

const Manifesto = () => {
  return (
    <>
      <div className="section s_white services-section opensource1 viewport-section">
        <div className="text-area">
          <a className="section-title" href="#open-source" id="open-source">
            Open source
          </a>
          <h1>COMMITED TO OPEN SOURCE AND ADVANCING KNOWLEDGE.</h1>
          <div className="text-wrap">
            <p>
              Open source is how we roll. 60% of our team where for a time in
              academia, where this is the norm. Today, open source is also
              becoming the norm for many companies that want to attract the
              highest levels of engineering talent.
            </p>
            <p>
              Why?{" "}
              <strong>
                Open source attracts minds from across the world and helps our
                customers build systems better and faster.
              </strong>{" "}
              Code sharing works.
            </p>
          </div>
        </div>
        <div className="image-holder image-holder hideresp">
          <img src={img12} alt="" />
        </div>
      </div>
      <div className="section s_white services-section biotech viewport-section">
        <div className="image-holder image-holder2">
          <img src={img13} alt="" />
        </div>
        <div className="text-wrap text-area2">
          <p>
            We employ researchers with strong academic backgrounds and ask them
            to keep pushing the envelope. Their sole focus is to identify future
            directions for programming languages, and we bring research to
            production faster than ever before. Tweagers are working on linear
            types, dependent types and mobile code that runs{" "}
            <strong>everywhere.</strong>
          </p>
        </div>
      </div>
      <div className="section s_white services-section opensource reverse viewport-section">
        <div className="image-holder image-holder3">
          <img src={img14} alt="" />
        </div>
        <div className="text-wrap text-area">
          <p>
            <strong>
              Our clients get easy access to some of the best minds in computer
              science.
            </strong>{" "}
            They also get Tweagers standing on the shoulders of giants who are
            right behind them. Knowledge sharing works.
          </p>
          <p>
            These commitments have helped us build strong, deep, and wide
            connections in open source projects that are changing the tech
            landscape. Below are just a few.
          </p>
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

const OpenSourcePage = () => {
  return (
    <Layout>
      <SEO title="Our open source projects" />
      <section className="section-area">
        <Manifesto />
        <Community title="Nix" className="s_red opensource">
          <p>
            Many thought leaders of the{" "}
            <a className="lined" href="https://nixos.org">
              Nix community
            </a>{" "}
            are Tweagers. Together with our clients, we contribute much of the
            technical roadmap, from better reproducibily and performance to new
            use cases and developer tools.
          </p>
        </Community>
        <Community title="Haskell" className="s_orange opensource3">
          <p>
            Tweagers are among the top contributors to GHC, a mature,
            state-of-the-art compiler for Haskell. Haskell is now recognized as
            the shortest path to correct, scalable code for industries where
            mistakes matter. We created language interop with Java, C, R,
            JavaScript, make Haskell run in your browser, and do the heavy
            lifting in your analytics pipelines.
          </p>
        </Community>
        <Community title="Bazel" className="s_grey opensource2">
          <p>
            We were among the first outside of Google to adopt Bazel. We are{" "}
            <a className="lined" href="https://bazel.build/experts.html">
              recognized community experts
            </a>
            {". "}
            We have contributed key features and new programming language
            support to Bazel. We did this for ourselves and now{" "}
            <Link to="/bazel" className="lined">
              we can do it for you
            </Link>
            {". "}
          </p>
        </Community>
        <div className="section s_white about-section opensource opensource4 viewport-section">
          <div className="text-wrap text-area w100">
            <p className="check">
              Check out our
              <a className="btn noarrow" href="https://github.com/tweag">
                Github page
              </a>
              or read our
              <Link to="/blog" className="btn noarrow">
                Blog
              </Link>
              to learn more
            </p>
          </div>
        </div>
      </section>
    </Layout>
  )
}

export default OpenSourcePage

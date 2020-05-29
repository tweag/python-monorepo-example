import React from "react"
import { Link } from "gatsby"

import CallToAction from "../components/cta"
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
          <h1>TWEAG KNOWS BAZEL.</h1>
          <div className="text-wrap">
            <p>
              <strong>
                Bazel builds billions of lines of source code, thousands of
                times a day at Google.
              </strong>{" "}
              When you build software with Bazel, you're using the same tool
              Google relies on to build their mission-critical infrastructure,
              services, and applications.
            </p>
            <p>
              That's impressive—but not everyone's a Google. There's a broad,
              growing community of Bazel users in more typical settings who
              build software that does remarkable things in commercial
              environments and in open source projects. Bazel builds software
              that's in your daily life, software that could save your life, and
              software that will change your life.
            </p>
            <p>
              We believe Bazel has the potential to be a leading build tool for
              a long time. For many, Bazel as your build tool isn't an if—it's a
              when. Bazel's tagline Build and test software of any size, quickly
              and reliably is accurate. It's also accurate that Bazel migrations
              and tuning can be challenging. That's why we're helping worldwide
              with Bazel's adoption, and with its ongoing development—at its
              core, and with complementary open source tooling.
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

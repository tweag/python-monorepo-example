import React from "react"

import CallToAction from "../../components/cta"
import Layout from "../../components/layout"
import SEO from "../../components/seo"

import imgVertical1 from "../../images/use_case_vertical1.svg"
import imgVertical2 from "../../images/use_case_vertical2.svg"
import img15 from "../../images/img15.svg"

const PfizerPage = () => {
  return (
    <Layout>
      <SEO title="Making genetic data accessible for drug discovery" />
      <section class="section-area">
        <div class="services-section use_case first viewport-section">
          <div class="text-area">
            <div class="section-title">Use case</div>
            <h1>MAKING GENETIC DATA ACCESSIBLE FOR DRUG DISCOVERY.</h1>
            <div class="text-wrap">
              <p class="use_case_excerpt">
                Tweag collaborated with Pfizer to enhance its search engine for
                human genetic data to help advance scientific discovery.
              </p>
              <p>
                Human genetic data holds the key to creating new medicines to
                treat disease. However, the amount of data doubles every seven
                months – this rapid growth makes it difficult for scientists to
                zero in on the specific information they need in drug discovery.
              </p>
              <p>
                To help solve this challenge, Pfizer created the Table of
                Everything (ToE): a dynamic internal database that catalogues
                almost 20,000 human genes and their related diseases and traits,
                and automatically updates weekly. It allows scientists to
                quickly access information related to a specific gene. Code
                sharing works.
              </p>
            </div>
          </div>
          <div class="image-holder image-holder">
            <img class="uc_rotate" src={imgVertical1} alt="" />
            <img src={imgVertical2} alt="" />
          </div>
        </div>
        <div class="services-section use_cases second viewport-section">
          <div class="image-holder image-holder2">
            <img src={img15} alt="" />
          </div>
          <div class="text-wrap text-area2">
            <p>
              Our commitment to knowledge sharing runs deeper than sharing code.
              We employ researchers with strong academic backgrounds and ask
              them to keep pushing the envelope. Their sole focus is to identify
              future directions for programming languages, and we bring research
              to production faster than ever before. Tweagers are working on
              linear types, dependent types and mobile code that runs{" "}
              <strong>everywhere.</strong>
            </p>
          </div>
        </div>
        <div class="about-section use_case bloc1 viewport-section">
          <div class="text-wrap text-area">
            <h1>FAST &amp; ITERATIVE COLLABORATION</h1>
            <p>
              Tweag used a fast, iterative approach to develop the ToE to meet
              Pfizer’s needs. Tweag engineers embedded with Pfizer’s R&D to
              iterate on a weekly basis. Each week, Tweag deployed a test
              version with Pfizer’s latest requests and changes. This rapid
              feedback loop enabled Pfizer to develop the ToE to meet their
              evolving needs, quickly and cost-efficiently.
            </p>
          </div>
        </div>
        <div class="about-section use_case bloc2 viewport-section">
          <div class="text-wrap text-area">
            <h1>SOFTWARE &amp; BIOINFORMATIC EXPERTS</h1>
            <p>
              Unlike typical software engineers, Tweag engineers have domain
              expertise in bioinformatics and biology. This gave Tweag engineers
              a richer understanding of Pfizer’s goals and needs, so they could
              recommend the right specs to achieve them. Tweag’s bioinformatic
              expertise saved Pfizer time and money in software development.
            </p>
          </div>
        </div>
        <div class="services-section use_case results viewport-section">
          <div class="text-area">
            <h1>RESULTS</h1>
            <div class="text-wrap">
              <ul>
                <li>
                  The Table of Everything was recognized with a prestigious
                  R&amp;D achievement award from Pfizer.
                </li>
                <li>
                  Today, many Pfizer scientists in early discovery use the
                  enhanced ToE to hunt for new potential targets to treat
                  diseases.
                </li>
                <li>
                  The enhanced ToE has also enabled Pfizer to collaborate with
                  scientists across the industry, contributing to the efforts of
                  several industry-wide genetic databases.
                </li>
              </ul>
            </div>
            <quote>
              <h3>
                I really value Tweag’s rapid prototyping and their creativity
                and willingness to try different things.
              </h3>
            </quote>
            <span class="text">
              <i class="icon-arrow-right1"></i> Eric Fauman, Senior Scientific
              Director, Head Integrative Biology, Pfizer
            </span>
          </div>
          <div class="image-holder image-holder">
            <img class="uc_rotate" src={imgVertical2} alt="" />
            <img src={imgVertical2} alt="" />
          </div>
        </div>
        <div className="section s_green section06a viewport-section">
          <CallToAction backdropIndex={8} />
        </div>
      </section>
    </Layout>
  )
}

export default PfizerPage

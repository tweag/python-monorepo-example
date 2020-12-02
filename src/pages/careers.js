import React from "react"

import { Link } from "gatsby"

import Layout from "../components/layout"
import img16 from "../images/img16.svg"
import img8 from "../images/img8.svg"
import img7 from "../images/img7.svg"

const TopPageContent = () => (
  <div className="services-section viewport-section">
    <div className="text-area">
      <a className="section-title" href="#services" id="services">
        Jobs
      </a>
      <h1>Interested in working together?</h1>
      <div className="text-wrap">
        <p>
          Join us to help shape the future of software engineering.
          <br />
          Visit our job offers{` `}
          <a href="https://boards.greenhouse.io/tweag">here</a>
          {` `}
          or send a direct email to our recruiting team at{` `}
          <a href="mailto:jobs@tweag.io">jobs@tweag.io</a>.
        </p>
      </div>
    </div>
    <div className="image-holder hideresp">
      <img src={img16} alt="" />
    </div>
  </div>
)

const DownPageContent = () => (
  <div className="method-area viewport-section">
    <div className="image-holder">
      <img src={img7} alt="" />
    </div>
    <div className="text-area">
      <div className="text-wrap careers">
        <p>
          Tweag is a global community of engineers passionate about solving
          today’s most interesting and difficult problems.
        </p>
        <p>
          As a remote-first company, we hire the best people wherever they live.
          Our horizontal structure gives engineers the autonomy to make their
          own decisions and handle a variety of responsibilities.
        </p>
        <p>
          We actively invest in
          {` `}
          <Link to="/opensource">
            open source communities and research projects
          </Link>
          .
        </p>
      </div>
    </div>
  </div>
)

const Jobs = () => {
  return (
    <div className="about-section viewport-section">
      <div className="text-area">
        <ul className="text-list">
          <li style={{ marginBottom: `1.5em` }}>
            We are always keen to hear from people interested in the following
            roles:
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Engineer
            <p>
              Tweag engineers are experienced with software development, data
              science, or with the associated infrastructure. Our engineers join
              teams in small startups or large multinationals around the world
              to help them realize software projects that go beyond the current
              state of the art.
              <br />
              <br />
              Maths, physics, computer science, geoscience, biology, arts, or
              nothing in particular: if you can produce quality code and use
              computers to solve real-world problems, you are at the right
              place.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Intern
            <p>
              Tweag interns work remotely or in our headquarters in Paris on
              research topics or new applications. The focus of an internship is
              to learn and to make headway with a real problem.
              <br />
              <br />
              Tweag proposes individually tailored projects and mentorship for
              each intern. Openings vary but usually appear from the end of the
              year until early spring.
            </p>
          </li>
          <li>
            <i className="icon-arrow-right1"></i> Fellow
            <p>
              Tweag Fellows are independent developers, programmers, researchers
              or enthusiasts that Tweag I/O supports.
              <br />
              <br />
              <Link to="blog/2020-02-14-os-fellowship">
                Open Source Fellows
              </Link>
              {` `}
              get financial support and mentorship for 12 weeks to bring their
              own ideas into practice for the benefit of the open source
              community. Application details can be found{` `}
              <a href="https://boards.greenhouse.io/tweag">here</a>. (Other
              Fellowships to be announced soon.)
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

class CareerPage extends React.Component {
  render() {
    return (
      <Layout>
        <div className="section s_white section-area">
          <TopPageContent />
          <DownPageContent />
          <Jobs />
        </div>
        <div className="line-sep"></div>
        <div className="section s_white section-wrap key_indus viewport-section">
          <div className="container">
            <div className="col-area">
              <h2 style={{ margin: `0`, lineHeight: `1.5em` }}>
                Interested in working together?
                <br /> Visit our job offers{` `}
                <a href="https://boards.greenhouse.io/tweag">here</a>
                {` `}
                or <br />
                send a direct email to our recruiting team at{` `}
                <a href="mailto:jobs@tweag.io">jobs@tweag.io</a>.
              </h2>
            </div>
          </div>
        </div>
      </Layout>
    )
  }
}

export default CareerPage

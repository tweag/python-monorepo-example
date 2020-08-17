import React from "react"

import Layout from "../components/layout"
import { submitForm } from "../components/form"

const ContactPage = () => {
  return (
    <Layout>
      <section className="section-area contact">
        <form
          id="contactform"
          name="contact"
          data-netlify-honeypot="bot-field"
          data-netlify="true"
          onSubmit={submitForm}
        >
          <div className="section s_white services-section viewport-section">
            <div className="text-area">
              <h1>
                <i></i> GET IN TOUCH.
              </h1>
            </div>
          </div>
          <div className="section s_yellow services-section contact-field viewport-section">
            <div className="text-wrap text-area">
              <label>Email*</label>
              <input
                name="email"
                type="text"
                onFocus="this.placeholder = ''"
                onBlur="this.placeholder = 'myemail@example.com'"
                placeholder="myemail@example.com"
              />
            </div>
            <div className="text-wrap text-area">
              <label>Message*</label>
              <textarea
                name="message"
                placeholder="Write your message"
              ></textarea>
              <button className="btn" type="submit">
                Send message
              </button>
              <p id="formmessage" style={{ visibility: `hidden` }}>
                {` `}
                Thank you for reaching out, we will soon reply to your request.
                {` `}
              </p>
            </div>
            <div className="line-sep"></div>
            <div className="text-wrap text-area">
              <label>Or send a direct email to our team</label>
              <p className="hello">
                <a href="mailto:hello@tweag.io">hello@tweag.io</a>
              </p>
            </div>
          </div>
          <div className="section s_white services-section contact-field viewport-section"></div>
        </form>
      </section>
      <div className="section s_white section-wrap services-section contact-addr viewport-section">
        <div className="text-area">
          <div className="col-area animation-wrap">
            <div className="col-row">
              <div className="info-col">
                <div className="block">
                  <div className="description">
                    <h3>PARIS</h3>EURL Tweag
                    <br />
                    207 Rue de Bercy
                    <br />
                    75012 Paris
                    <br />
                    France
                  </div>
                </div>
              </div>
              <div className="info-col">
                <div className="block">
                  <div className="description">
                    <h3>LONDON</h3>Tweag UK
                    <br />
                    Devonshire House,
                    <br />
                    60 Goswell Road
                    <br />
                    London, EC1M 7AD
                    <br />
                    United Kingdom
                  </div>
                </div>
              </div>
              <div className="info-col">
                <div className="block">
                  <div className="description">
                    <h3>CYPRUS</h3>Tweag I/O Limited
                    <br />
                    Servias street 1<br />
                    Engomi 2412
                    <br />
                    Nicosia
                    <br />
                    Cyprus
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </Layout>
  )
}

export default ContactPage

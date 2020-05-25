import React from "react"

import Layout from "../components/layout"

const ContactPage = () => {
  return (
    <Layout>
      <section class="section-area contact">
        <div class="section s_white services-section viewport-section">
          <div class="text-area">
            <h1>
              <i></i> GET IN TOUCH.
            </h1>
          </div>
        </div>
        <div class="section s_yellow services-section contact_field viewport-section">
          <div class="text-wrap text-area">
            <label>Email*</label>
            <input type="text" placeholder="myemail@xyz.com" />
            <label>Company*</label>
            <input type="text" placeholder="My Company" />
            <label>Website*</label>
            <input type="text" placeholder="http://" />
          </div>
        </div>
        <div class="section s_white services-section contact_field viewport-section">
          <div class="text-wrap text-area">
            <label>Your message</label>
            <textarea>Write your message</textarea>
            <a class="btn" href="#">
              Send message
            </a>
          </div>
        </div>
        <div class="line_sep"></div>
      </section>
      <div class="section s_white section-wrap services-section contact_addr viewport-section">
        <div class="text-wrap text-area">
          <label>Or send a direct email to our team</label>
          <p class="hello">
            <a href="mailto:hello@tweag.io">hello@tweag.io</a>
          </p>
        </div>
        <div class="text-area">
          <div class="col-area animation-wrap">
            <div class="col-row">
              <div class="info-col">
                <div class="block">
                  <div class="description">
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
              <div class="info-col">
                <div class="block">
                  <div class="description">
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
              <div class="info-col">
                <div class="block">
                  <div class="description">
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

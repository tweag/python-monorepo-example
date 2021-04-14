/** @jsx jsx */
import { jsx, Grid, Box, Text, Label, Input, Textarea, Flex } from "theme-ui"
import { useState, Fragment } from "react"

import { DefaulLayout as Layout } from "../layouts"
import { SEO } from "../components"

const content = {
  locations: [
    {
      city: `Paris`,
      address: (
        <Fragment>
          EURL Tweag
          <br />
          207 Rue de Bercy
          <br />
          75012 Paris
          <br />
          France
        </Fragment>
      ),
    },
    {
      city: `London`,
      address: (
        <Fragment>
          Tweag UK
          <br />
          Devonshire House,
          <br />
          60 Goswell Road
          <br />
          London, EC1M 7AD
          <br />
          United Kingdom
        </Fragment>
      ),
    },
    {
      city: `Cyprus`,
      address: (
        <Fragment>
          Tweag I/O Limited
          <br />
          Servias street 1<br />
          Engomi 2412
          <br />
          Nicosia
          <br />
          Cyprus
        </Fragment>
      ),
    },
  ],
}

const CustomLabel = ({ children }) => (
  <Label sx={{ fontWeight: [`bold`], fontSize: [`16px`, `16px`, `27px`] }}>
    {children}
  </Label>
)

const ContactPage = () => {
  const [messageSent, setMessageSent] = useState(false)
  const [formMessage, setFormMessage] = useState(``)

  function submitForm(e, onMessageSent) {
    console.log(`submitting now`)
    e.preventDefault()

    // Create the new request
    const portalid = `6929938`
    const formid = `d7302341-7237-4d17-a810-1f1148846f5e`
    const url = `https://api.hsforms.com/submissions/v3/integration/submit/${portalid}/${formid}`
    const form = document.querySelector(`#contactform`)
    const formData = new FormData(form)
    const unwantedFields = [`bot-field`, `form-name`] // these fields are added for netlify
    const entries = Array.from(formData.entries())
    const data = {
      submittedAt: Date.now().toString(),
      fields: entries
        .filter(pair => {
          return !unwantedFields.includes(pair[0])
        })
        .map(pair => {
          return { name: pair[0], value: pair[1] }
        }),
    }

    const xhr = new XMLHttpRequest()
    xhr.open(`POST`, url)
    // Sets the value of the 'Content-Type' HTTP request headers to 'application/json'
    xhr.setRequestHeader(`Content-Type`, `application/json`)

    xhr.onreadystatechange = function () {
      if (xhr.readyState == 4 && xhr.status == 200) {
        setFormMessage(
          `Thank you for reaching out, we will soon reply to your request.`
        )
        onMessageSent()
      } else if (xhr.readyState == 4 && xhr.status == 400) {
        setFormMessage(`Please fill in a valid email address and a message.`)
        console.log(xhr.responseText) // Returns a 400 error if the submission is rejected.
      } else if (xhr.readyState == 4 && xhr.status == 403) {
        setFormMessage(`No connection to our systems, please try later.`)
        console.log(xhr.responseText) // Returns a 403 error if the portal isn't allowed to post submissions.
      } else if (xhr.readyState == 4 && xhr.status == 404) {
        setFormMessage(`No connection to our systems, please try later.`)
        console.log(xhr.responseText) // Returns a 404 error if the formGuid isn't found
      }
    }

    xhr.send(JSON.stringify(data))
  }

  return (
    <Layout>
      <SEO title="Contact us." />
      <div
        className="section s_white viewport-section transition-section"
        sx={{
          pt: [`65px`, `65px`, `135px`],
        }}
      >
        <Text
          className="transition-section__transition--slide-fade-in bottom-in"
          sx={{
            minHeight: `100px`,
            textTransform: `uppercase`,
            fontSize: [`34px`, `34px`, `66px`],
            fontWeight: 700,
            lineHeight: [1, 1],
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            mt: [`20px`, `20px`, `50px`],
          }}
        >
          Contact us
        </Text>
      </div>
      <div
        className="section s_yellow viewport-section transition-section"
        sx={{
          mt: [`75px`, `75px`, `40px`],
        }}
      >
        <form
          id="contactform"
          name="contact"
          data-netlify-honeypot="bot-field"
          data-netlify="true"
          onSubmit={e => {
            if (!messageSent) submitForm(e, () => setMessageSent(true))
            else {
              e.preventDefault()
              setFormMessage(
                `A message has been already sent. Please refresh the page if you want to send a new message`
              )
            }
          }}
        >
          <Grid
            sx={{
              pt: [`60px`],
              pb: [`75px`, `75px`, `55px`],
              px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
              width: [`100%`, `100%`, `70%`, `70%`, `70%`, `70%`, `52%`],
            }}
          >
            <Grid
              className="transition-section__transition--slide-fade-in bottom-in"
              gap={[`100px`]}
            >
              <Grid gap={[`40px`]}>
                <CustomLabel>Email*</CustomLabel>
                <Input
                  name="email"
                  type="text"
                  placeholder="myemail@example.com"
                  sx={{
                    fontSize: [`24px`, `24px`, `42px`],
                    border: [0],
                    padding: [`0 !important`],
                    "::placeholder": {
                      opacity: 0.3,
                    },
                    "&:focus": {
                      outline: `none`,
                    },
                  }}
                />
              </Grid>
              <Grid gap={[`40px`]}>
                <CustomLabel>Message*</CustomLabel>
                <Textarea
                  name="message"
                  placeholder="Write your message"
                  sx={{
                    fontSize: [`24px`, `24px`, `42px`],
                    border: [0],
                    padding: [`0 !important`],
                    minHeight: [`300px`],
                    "::placeholder": {
                      opacity: 0.3,
                    },
                    "&:focus": {
                      outline: `none`,
                    },
                  }}
                />
              </Grid>
            </Grid>
            <Grid className="transition-section__transition--slide-fade-in bottom-in">
              <button
                className="button button-secondary button-medium pre-arrow-right min-5__button-large"
                type="submit"
                sx={{
                  justifySelf: `start`,
                  fontSize: [`18px`, `18px`, `24px !important`],
                }}
              >
                Send message
              </button>
            </Grid>
            <Text
              className="transition-section__transition--slide-fade-in bottom-in"
              sx={{
                minHeight: [`100px`],
                fontSize: [`18px`, `18px`, `27px`],
              }}
            >
              {formMessage}
            </Text>
            <Text
              className="transition-section__transition--slide-fade-in bottom-in"
              sx={{
                fontSize: [`16px`, `16px`, `27px`],
                lineHeight: [1.2],
                fontWeight: [400],
              }}
            >
              <span sx={{ fontWeight: 700 }}>
                Or send a direct email to our team
              </span>
              {` `}
              <a sx={{ display: `block` }} href="mailto:hello@tweag.io">
                hello@tweag.io
              </a>
            </Text>
          </Grid>
        </form>
      </div>
      <div className="section s_white">
        <Flex
          className="viewport-section transition-section"
          sx={{
            pt: [`100px`, `100px`, `100px`, `100px`, `100px`, `80px`],
            pb: [`100px`],
            pl: [`0px`, `0px`, `45px`, `45px`, `45px`, `45px`, `105px`],
            flexWrap: `wrap`,
            width: [`100%`],
          }}
        >
          {content.locations.map(({ city, address }, i) => (
            <Box
              className={`transition-section__transition--slide-fade-in bottom-in delayed-${
                (i + 1) * 3
              }`}
              key={city}
              sx={{
                fontSize: [`18px`],
                lineHeight: [`26px`],
                mt: [`30px`, `30px`, `30px`, `30px`, `30px`, 0],
                mb: [0, 0, `30px`],
                pr: [`80px`],
                pl: [`15px`],
              }}
            >
              <Text
                sx={{
                  textTransform: `uppercase`,
                  fontWeight: 700,
                  fontSize: [`42px`, `42px`, `66px`],
                  lineHeight: [1.1],
                  mb: [0, 0, 0, 0, 0, `15px`],
                }}
              >
                {city}
              </Text>
              {address}
            </Box>
          ))}
        </Flex>
      </div>
    </Layout>
  )
}

export default ContactPage

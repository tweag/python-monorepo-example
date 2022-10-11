import React from "react"
import { Divider, Box, Text } from "theme-ui"
import { DefaulLayout as Layout } from "../layouts"
import { SEO } from "../components"

const PrivacyPolicy = () => {
  return (
    <Layout>
      <SEO title="Privacy Policy" />
      <div
        className="section s_white viewport-section transition-section"
        sx={{
          pt: [`65px`, `65px`, `135px`],
        }}
      >
        <Text
          as="div"
          className="section s_black transition-section__transition--slide-fade-in bottom-in"
          sx={{
            minHeight: `7rem`,
            textTransform: `uppercase`,
            fontSize: `6rem`,
            fontWeight: 700,
            backgroundColor: `#051017`,
            color: `#ffffff`,
            lineHeight: [1, 1],
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `300px`],
            mt: `5rem`,
            py: `20rem`,
          }}
        >
          Privacy Policy
        </Text>
      </div>
      <div className="section s_white">
        <Box
          sx={{
            pt: `2rem`,
            pb: `8rem`,
            px: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `300px`],
          }}
        >
          <Box sx={{ pb: `2rem`, fontSize: `3.5rem`, fontWeight: 800 }}>
            Introduction
          </Box>
          <Box sx={{ py: `1rem`, fontSize: `1.5rem` }}>
            EURL Tweag Sarl (referred to as “Tweag” or “we”) is part of Modus
            Create and respects individual’s rights surrounding the collection
            and usage of their personal identifiable information (“data”). This
            privacy policy serves as a declaration of how we collect data, why
            we collect, and your rights as a user (“you,” “user,” “visitor”)
            across the website www.tweag.io, advertising, and offline activities
            during the course of business.
            <br />
            <Text as="div" sx={{ pt: `1.5rem` }}>
              If you have any questions, cannot use the automated unsubscribe
              options, or receive communications from Tweag after opting out,
              contact us immediately by emailing dataprotection@tweag.io, or by
              mail: 207 Rue de Bercy, Paris, 75012, France.
            </Text>
          </Box>
          <Divider />

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Data Collection
          </Box>
          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Methods
          </Box>

          <Box sx={{ fontSize: `1.5rem` }}>
            Tweag captures user information via the following methods. For a
            more detailed description, please see below in Tracking
            Technologies: The opt-in forms on our website and digital
            communications, including email newsletter or Email. This may also
            include other distinct and specialized sites for events or service
            offerings.
            <ul>
              <li>
                Cookies and internet access logs. These indirect methods may
                contain previous URLs visited, times and dates of visits, and
                information about computer hardware and software you use.
              </li>
              <li>Sign-up sheets or lead capturing applications at events.</li>
            </ul>
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Types of Data Collected
          </Box>

          <Box sx={{ fontSize: `1.5rem` }}>
            The data collected may include the following information:
            <ul>
              <li>
                Subscribers’ contact information (name, address, company, title,
                phone, fax, email, web, social media handles)
              </li>
              <li>
                The subscriber’s work title, company, and industry they work in
              </li>
              <li>
                A selection of topics of interest for future communications
              </li>
              <li>
                The user’s behavior on the Tweag website and with Tweag
                communications (email, sales calls)
              </li>
              <li>
                The user’s referral path (i.e., email, social media, organic
                search)
              </li>
              <li>
                Subscriber’s anonymized geographic and demographic location
              </li>
            </ul>
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Retention Period
          </Box>

          <Box sx={{ fontSize: `1.5rem` }}>
            Cookies gathered through Google properties are subject to Google’s
            storage terms, read more here:
            https://developers.google.com/analytics/devguides/collection/analyticsjs/cookie-usage.
            <br />
            <Text as="div" sx={{ py: `1.5rem` }}>
              For user-volunteered information on web-based forms that Modus
              gathers, we will store contact information until the user opts
              out, requests deletion, or the retention period is met. For more
              information on the retention period please contact us at
              dataprotection@moduscreate.com
            </Text>
            <Divider />
          </Box>

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Purposes for Data Collected
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `1.5rem` }}>
            We collect data for the following purposes:
            <ul>
              <li>
                To improve the functionality of the website by avoiding
                duplicate data entry, facilitating navigation, and increasing
                the relevance of content
              </li>
              <li>
                To store user preferences and profile information used to
                personalize content and manage our relationship with you and our
                clients
              </li>
              <li>
                To collect contact information for potential clients and
                employees
              </li>
              <li>
                To measure visitors’ interest in content, blog articles,
                partnerships, and Tweag service offerings
              </li>
            </ul>
          </Box>

          <Divider />

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Consent
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            How do I give my consent?
          </Box>

          <Box sx={{ fontSize: `1.5rem` }}>
            By closing the modal popup and continuing to use the Tweag site, you
            acknowledge you have read this privacy policy and accept the terms
            of data collection and usage outlined herein. When you provide
            personal information and submit a digital form on our website, enter
            information into a form at an event, or present a business card, you
            are consenting to the collection of your information.
            <br />
            <Text as="div" sx={{ py: `1.5rem` }}>
              Users who submit a form via our website could receive a ‘Thank
              You’ message acknowledging your information has been received.
            </Text>
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            How do I withdraw my consent?
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `1.5rem` }}>
            We do not offer an option for opt-out of browser cookies, browsers
            provide users with control over cookies. You can receive alerts when
            cookies are being used, learn information on the duration of
            cookies, what server your cookies are returned to, and have the
            option to accept or reject cookies through your browser’s settings.
            <br />
            <Text as="div" sx={{ pt: `1.5rem` }}>
              All emails from Tweag include an “Unsubscribe” link, which will
              direct the user to a page where they can cease future
              communications.
            </Text>
            <br />
            <Text as="div" sx={{ pb: `1.5rem` }}>
              If you would like to withdraw your consent or have your personal
              data deleted, you may contact us at any time by emailing
              dataprotection@tweag.io, mailing us at: 207 Rue de Bercy, Paris,
              75012, France. Please use the same contact process to file
              requests based on the CCPA, GDPR, LGPD, or other applicable data
              protection legislation with the Tweag team.
            </Text>
          </Box>

          <Divider />

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Overseas Transfer
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `1.5rem` }}>
            Information that we collected may be stored or processed in and
            transferred between any of the countries in which Tweag, its
            partners, employees, and contractors have offices. By submitting
            your information on the Tweag website or any of our other
            activities, you expressly agree to such transfer in accordance with
            this Privacy Policy.
          </Box>
          <Divider />
          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Third-Party Data Processing Platforms
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Google
          </Box>

          <Box sx={{ fontSize: `1.5rem` }}>
            We use Google Analytics, which uses cookies to track user behavior
            on our website to improve site experience, optimize content, and
            retarget of digital advertising. By dismissing the alert and
            continuing to browse our site, you consent to use cookies.
            <br />
            <Text as="div" sx={{ py: `1.5rem` }}>
              We have enabled IP Anonymization within the Google suite to
              prevent the capturing of explicit IP addresses. For more
              information on Google, read their data processing terms:
              https://privacy.google.com/businesses/processorterms/
            </Text>
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Hubspot
          </Box>

          <Box sx={{ fontSize: `1.5rem` }}>
            HubSpot code is embedded within the forms on the Tweag site. When
            you submit a form, your data is stored in HubSpot’s databases and
            applications. This data is secured behind a firewall and hosted by
            HubSpot.
            <br />
            <Text as="div" sx={{ py: `1.5rem` }}>
              Learn more about the cookies set by HubSpot when a user visits our
              site here:
              https://knowledge.hubspot.com/articles/KCS_Article/Reports/What-cookies-does-HubSpot-set-in-a-visitor-s-browser
            </Text>
          </Box>
          <Divider />

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Security
          </Box>
          <Box sx={{ pb: `1.5rem`, fontSize: `1.5rem` }}>
            Tweag’s long-standing commitment to protect the privacy of
            information our visitors and clients entrust to us is essential to
            our goal as a leading global provider of professional services.
            Protecting confidentiality and security of private information has
            always been an integral part of how we conduct business worldwide.
          </Box>

          <Divider />

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Changes to this Policy
          </Box>
          <Box sx={{ pb: `1.5rem`, fontSize: `1.5rem` }}>
            Tweag reserves the right to modify this privacy policy at any time.
            Changes will take effect immediately upon their posting on this
            page, and users will not be expressly notified of any future changes
            to the privacy policy.
          </Box>

          <Divider />
          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Tracking Technologies
          </Box>

          <Box sx={{ pb: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Cookies
          </Box>
          <Box sx={{ fontSize: `1.5rem` }}>
            Modus’ site uses cookies to save and retrieve information. These
            cookies identify users in an anonymized way. The settings of what
            cookies your browser retains and uses can be set within the
            browser’s settings menu.
          </Box>

          <Box sx={{ py: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Display Advertising
          </Box>
          <Box sx={{ fontSize: `1.5rem` }}>
            We use third-party vendors, including Google, to inform, serve, and
            optimize display ads based on visits and activity on our website.
            Using Google Analytics and Google AdWords, we engage in remarketing;
            retargeting content to users based on their behavior on our site.
            Please refer to Google’s site for more information on their
            remarketing services:
            https://support.google.com/analytics/answer/2611268.
          </Box>

          <Box sx={{ py: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Web Beacons
          </Box>
          <Box sx={{ fontSize: `1.5rem` }}>
            Our website contains web beacons, otherwise known as pixels. These
            tools are used to collect aggregated metrics about the performance
            of our advertising on Social Media platforms, specifically Facebook
            and Twitter, and the performance of our email communications. This
            information is anonymized and used to gauge the effectiveness of
            marketing campaigns in aggregate. No personal information will be
            collected by these tools.
          </Box>

          <Box sx={{ py: `1.5rem`, fontSize: `2rem`, fontWeight: 600 }}>
            Email
          </Box>
          <Box sx={{ pb: `1.5rem`, fontSize: `1.5rem` }}>
            Users must opt-in on our website or at an event via a sign-up sheet
            to be enlisted to receive email communications from Tweag. All
            emails from Tweag include an “Unsubscribe” link, which will direct
            the user to a page where they can cease future communications.
          </Box>

          <Divider />

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Disclosure and Information Sharing
          </Box>
          <Box sx={{ pb: `1.5rem`, fontSize: `1.5rem` }}>
            We may disclose your personal information if we are required by law.
            Tweag will never sell personal data to a third party under any
            circumstances or share data for purposes that the user has not
            endorsed.
          </Box>

          <Divider />

          <Box sx={{ py: `1.5rem`, fontSize: `3.2rem`, fontWeight: 800 }}>
            Rights
          </Box>
          <Box sx={{ fontSize: `1.5rem` }}>
            As required by applicable legislation, Tweag will respect your
            Privacy rights. Such rights include, but are not limited to: Right
            to review, change or delete your personal information, Right to
            object to marketing communications, Right to restrict our processing
            of your personal information and a Right to complain to the
            competent data protection authority. For more information regarding
            your rights, please contact us at dataprotection@tweag.io
          </Box>
        </Box>
      </div>
    </Layout>
  )
}

export default PrivacyPolicy

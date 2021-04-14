/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"
import { Grid, Box, Text, Flex } from "theme-ui"

import { DefaulLayout as Layout } from "../layouts"
import { SectionHeading, CallToActionFooter, SEO } from "../components"

import img12 from "../images/img12.svg"

const Manifesto = () => {
  return (
    <div className="section s_white">
      <Grid
        className="viewport-section transition-section"
        columns={[1, 1, 4, 4, 4, 4, 4, 4]}
        gap={[`15px`, `15px`, `30px`]}
        sx={{
          pt: [`60px`, `60px`, `130px`, `130px`, `130px`, `130px`, `160px`],
          pb: [`35px`, `35px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1"
          gap={[`35px`, `35px`]}
          sx={{
            px: [`15px`, `15px`, `0px`],
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            gridColumnStart: [`auto`, `auto`, 1],
            gridColumnEnd: [`auto`, `auto`, 4],
            gridAutoRows: [`max-content`],
            width: [`100%`, `100%`, `100%`, `100%`, `100%`, `100%`, `80%`],
          }}
        >
          <SectionHeading
            customSx={{
              justifySelf: `start`,
              alignSelf: `start`,
            }}
          >
            Bazel
          </SectionHeading>
          <Text
            sx={{
              fontSize: [`34px`, `34px`, `66px`],
              fontWeight: [700],
              lineHeight: [1],
              textTransform: `uppercase`,
              minHeight: [
                `100px`,
                `100px`,
                `100px`,
                `100px`,
                `100px`,
                `100px`,
                `auto`,
              ],
              mt: [`5px`, `5px`, `15px`],
            }}
          >
            Optimize your Bazel implementation
          </Text>
          <Grid
            columns={1}
            gap={[`15px`]}
            sx={{
              fontSize: [`18px`, `18px`, `27px`],
              lineHeight: [1.2, 1.2, `35px`],
              maxWidth: [`100%`, `100%`, `90%`, `90%`, `90%`, `90%`, `100%`],
            }}
          >
            {[
              <Fragment key={0}>
                Tweag is the software innovation lab driving Bazel’s worldwide
                adoption and ongoing development. As Google’s first Bazel
                Community Expert, we can help you optimize every phase of your
                Bazel implementation, from preparing for migration to improving
                your existing setup.
              </Fragment>,
              <Fragment key={1}>
                We can answer your toughest Bazel questions. Combining rich
                Bazel skills with deep technology expertise, Tweag offers rare
                insight into Bazel&rsquo;s capabilities (and challenges) across
                diverse environments.
              </Fragment>,
            ].map((para, idx) => (
              <Text key={idx}>{para}</Text>
            ))}
          </Grid>
        </Grid>
        <Box
          className="transition-section__transition--slide-fade-in right-in only-above-1 delayed"
          sx={{
            alignSelf: `start`,
            gridColumnStart: [`auto`, `auto`],
            gridColumnEnd: [`auto`, `auto`],
            marginTop: [`0px`, `0px`, `200px`, `200px`, `200px`],
            marginLeft: [`auto`],
            width: [`100%`, `100%`, `100%`],
            display: [`none`, `none`, `flex`],
            justifyContent: `flex-end`,
          }}
        >
          <img
            sx={{ width: [`100%`, `100%`, `87%`, `87%`, `87%`, `87%`, `80%`] }}
            src={img12}
            alt=""
          />
        </Box>
      </Grid>
    </div>
  )
}

const Community = ({ title, bgColor, children, bgImage }) => {
  return (
    <Flex
      className={`section s_${bgColor} viewport-section transition-section`}
      sx={{
        flexWrap: `wrap`,
        pt: [`40px`, `40px`, `70px`],
        pb: [`40px`, `40px`],
        px: [`15px`, `15px`, `5%`],
      }}
    >
      <Text
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
        sx={{
          flexBasis: [`100%`, `100%`, `30%`],
          fontSize: [`34px`, `34px`, `66px`],
          fontWeight: [700],
          lineHeight: [1],
          minHeight: [`100px`],
          mb: [`20px`],
          textTransform: `uppercase`,
        }}
      >
        {title}
      </Text>
      <Box
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
        sx={{
          flexBasis: [`100%`, `100%`, `65%`, `65%`, `65%`, `65%`, `50%`],
          fontSize: [`18px`, `18px`, `27px`],
          lineHeight: [1, 1, 1.2],
          pl: [`auto`, `auto`, `100px`],
        }}
      >
        {children}
      </Box>
      {bgImage && (
        <img
          src={bgImage}
          style={{
            position: `absolute`,
            top: `10px`,
            left: `10px`,
            width: `300px`,
            opacity: 0.1,
          }}
        />
      )}
    </Flex>
  )
}

const BazelPage = () => {
  return (
    <Layout
      fullPageFooter
      footer={
        <Box className="section viewport-section s_white transition-section">
          <CallToActionFooter
            title={`Ready to achieve your big vision?`}
            backdropVariant={3}
            transitionClass={`transition-section__transition--slide-fade-in`}
            customWrapperSx={{
              py: [`40px`, `40px`, `60px`],
            }}
          />
        </Box>
      }
    >
      <SEO title="Bazel" />
      <Manifesto />
      <Community title="Ready" bgColor="red">
        <p>
          Get a head start on your Bazel migration with Tweag’s Readiness
          Assessment. We&rsquo;ll document expected improvements, potential
          challenges and required resources to help you plan for a successful
          migration to a Bazel build system.
        </p>
      </Community>
      <Community title="Tune" bgColor="orange">
        <p>
          Harness Bazel&rsquo;s power after migration. We&rsquo;ll help you tune
          your Bazel to enjoy dramatic improvements in productivity and
          performance. From improving cache hits to writing custom rules, we
          have experience adapting Bazel to your needs. We&rsquo;ve also been
          successful advocates for getting solutions to our client&rsquo;s
          use-case requirements incorporated into core Bazel.
        </p>
      </Community>
      <Community title="Result" bgColor="grey">
        <p>We have worked with clients across a range of industries:</p>
        <p className="tab">
          <i className="icon-arrow-right1">
            {` `}
            In silico simulations of human physiology
          </i>
          <br />
          <i className="icon-arrow-right1">
            {` `}
            Novel static analysis tools for the autopilot software in autonomous
            taxis
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
    </Layout>
  )
}

export default BazelPage

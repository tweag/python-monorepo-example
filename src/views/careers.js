/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"
import { Box, Text, Grid } from "theme-ui"
import { Link } from "gatsby"

import img16 from "../images/img16.svg"
import img7 from "../images/img7.svg"
import img8 from "../images/img8.svg"

import { DefaulLayout as Layout } from "../layouts"
import { SEO, SectionHeading, Divider } from "../components"

const content = {
  title: `Jobs`,
  headline: `Interested in working together?`,
  firstParas: [
    <Fragment key={0}>
      <p>
        Join us to help shape the future of software engineering.
        <br />
        Visit our job offers{` `}
        <a href="https://boards.greenhouse.io/tweag">here</a>
        {` `}
        or send a direct email to our recruiting team at{` `}
        <a href="mailto:jobs@tweag.io">jobs@tweag.io</a>.
      </p>
    </Fragment>,
  ],
  secondParas: [
    <Fragment key={0}>
      Tweag is a global community of engineers passionate about solving today’s
      most interesting and difficult problems.
    </Fragment>,
    <Fragment key={1}>
      As a remote-first company, we hire the best people wherever they live. Our
      horizontal structure gives engineers the autonomy to make their own
      decisions and handle a variety of responsibilities.
    </Fragment>,
    <Fragment key={2}>
      We actively invest in
      {` `}
      <Link to="/opensource">
        open source communities and research projects
      </Link>
      .
    </Fragment>,
  ],
  roles: [
    {
      title: `Engineer`,
      description: (
        <Fragment key={0}>
          Tweag engineers are experienced with software development, data
          science, or with the associated infrastructure. Our engineers join
          teams in small startups or large multinationals around the world to
          help them realize software projects that go beyond the current state
          of the art.
          <br />
          <br />
          Maths, physics, computer science, geoscience, biology, arts, or
          nothing in particular: if you can produce quality code and use
          computers to solve real-world problems, you are at the right place.
        </Fragment>
      ),
    },
    {
      title: `Intern`,
      description: (
        <Fragment key={0}>
          Tweag interns work remotely or in our headquarters in Paris on
          research topics or new applications. The focus of an internship is to
          learn and to make headway with a real problem.
          <br />
          <br />
          Tweag proposes individually tailored projects and mentorship for each
          intern. Openings vary but usually appear from the end of the year
          until early spring.
        </Fragment>
      ),
    },
    {
      title: `Fellow`,
      description: (
        <Fragment key={0}>
          Tweag Fellows are independent developers, programmers, researchers or
          enthusiasts that Tweag I/O supports.
          <br />
          <br />
          <Link to="blog/2020-02-14-os-fellowship">Open Source Fellows</Link>
          {` `}
          get financial support and mentorship for 12 weeks to bring their own
          ideas into practice for the benefit of the open source community.
          Application details can be found{` `}
          <a href="https://boards.greenhouse.io/tweag">here</a>. (Other
          Fellowships to be announced soon.)
        </Fragment>
      ),
    },
  ],
}

export default function Careers() {
  return (
    <Layout>
      <SEO title="Careers" />
      <div className="section s_white">
        <Grid
          className="viewport-section transition-section"
          columns={[1, 1, 4, 4, 4, 4, 4, 4]}
          gap={[`15px`, `15px`, `30px`]}
          sx={{
            pt: [`60px`, `60px`, `130px`, `130px`, `130px`, `130px`, `160px`],
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
              {content.title}
            </SectionHeading>
            <Text
              as="div"
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
                width: [`100%`, `100%`, `90%`],
              }}
            >
              {content.headline}
            </Text>
            <Grid
              columns={1}
              gap={[`15px`]}
              sx={{
                fontSize: [`18px`, `18px`, `27px`],
                lineHeight: [1.2, 1.2, `35px`],
                maxWidth: [`100%`, `100%`, `90%`, `90%`, `90%`, `90%`, `90%`],
              }}
            >
              {content.firstParas.map((para, idx) => (
                <Text as="div" key={idx}>
                  {para}
                </Text>
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
              sx={{
                width: [`100%`, `100%`, `87%`, `87%`, `87%`, `87%`, `80%`],
              }}
              src={img16}
              alt=""
            />
          </Box>
        </Grid>
        <Grid
          className="section s_white transition-section viewport-section"
          columns={[1, 1, 4]}
          gap={[`30px`, `30px`]}
          sx={{
            pt: [`60px`, `60px`, `40px`],
          }}
        >
          <Box
            className="transition-section__transition--slide-fade-in left-in only-above-1"
            sx={{
              width: [`200px`, `200px`, `100%`],
              alignSelf: [`center`, `center`, `center`],
            }}
          >
            <img
              sx={{
                width: `100%`,
              }}
              src={img7}
              alt=""
            />
          </Box>
          <Grid
            className="transition-section__transition--slide-fade-in bottom-in only-above-1 delayed"
            sx={{
              pr: [`15px`, `15px`, `0px`],
              // pl: [`15px`, `15px`, `0px`],
              pl: [`15px`, `15px`, `180px`],
              // marginRight: [`auto`, `auto`, `auto`],
              my: [`0px`, `0px`, `0px`, `auto`],
              maxWidth: [
                `100%`,
                `100%`,
                `93.3333333%`,
                `93.3333333%`,
                `93.3333333%`,
                `93.3333333%`,
                `65.5%`,
              ],
              gridColumnStart: [`auto`, `auto`, 2],
              gridColumnEnd: [`auto`, `auto`, 5],
              gridAutoRows: [`min-content`],
              fontSize: [`16px`, `16px`, `24px`],
              lineHeight: [1.2, 1.2, `35px`],
            }}
            gap={[`20px`]}
          >
            {content.secondParas.map((para, idx) => (
              <Text as="div" key={idx}>
                {para}
              </Text>
            ))}
          </Grid>
        </Grid>
        <Grid
          className="transition-section viewport-section"
          columns={[1, 1, 2]}
          sx={{
            pt: [`76px`, `76px`, `126px`],
            pb: [`20px`],
          }}
        >
          <Grid
            sx={{
              pr: [`15px`, `15px`, `0px`],
              pl: [
                `15px`,
                `15px`,
                `50px`,
                `50px`,
                `50px`,
                `50px`,
                `50px`,
                `120px`,
              ],
            }}
            gap={[`25px`]}
            className="transition-section__transition--slide-fade-in bottom-in only-above-1"
          >
            <Text
              as="div"
              sx={{
                fontSize: [`18px`, `18px`, `34px`],
                lineHeight: [`22px`, `22px`, 1.1],
                fontWeight: [700],
                mb: [0, 0, `25px`],
              }}
            >
              We are always keen to hear from people interested in the following
              roles:
            </Text>
            <Grid gap={[`30px`]}>
              {content.roles.map(({ title, description }) => (
                <Grid key={title} gap={[`20px`]}>
                  <Text
                    as="div"
                    sx={{
                      fontWeight: [700],
                      fontSize: [`18px`, `18px`, `34px`],
                      lineHeight: [`22px`, `22px`, 1.1],
                    }}
                  >
                    <i className="icon-arrow-right1"></i> {title}
                  </Text>
                  <Text
                    as="div"
                    sx={{
                      fontWeight: [400],
                      fontSize: [`18px`, `18px`, `24px`],
                      lineHeight: [`22px`, `22px`, 1.1],
                      mx: [`6px`],
                    }}
                  >
                    {description}
                  </Text>
                </Grid>
              ))}
            </Grid>
            <Grid></Grid>
          </Grid>
          <Box
            className="transition-section__transition--slide-fade-in right-in only-above-1 delayed"
            sx={{
              width: [`200px`, `200px`, `100%`],
              justifySelf: `end`,
              alignSelf: `center`,
              display: `flex`,
              justifyContent: `flex-end`,
            }}
          >
            <img
              sx={{
                width: [`100%`, `100%`, `75%`],
              }}
              src={img8}
              alt=""
            />
          </Box>
        </Grid>
        <Divider
          customSx={{
            mx: [`20px`, `20p`, `60px`],
            mt: [`60px`],
            mb: [`20px`],
          }}
        />
        <div className="transition-section viewport-section">
          <Text
            as="div"
            className="transition-section__transition--slide-fade-in bottom-in only-above-1"
            sx={{
              px: [`15px`, `15px`, `110px`, `110px`, `110px`, `110px`, `135px`],
              pt: [0, 0, `110px`, `110px`, `110px`, `50px`],
              pb: [`30px`, `30px`, `130px`, `130px`, `130px`, `70px`],
              fontSize: [
                `24px`,
                `24px`,
                `34px`,
                `34px`,
                `34px`,
                `34px`,
                `42px`,
              ],
              fontWeight: 700,
              lineHeight: [`1.5em`, `1.5em`],
              textTransform: `uppercase`,
              textAlign: [`start`, `start`, `center`],
            }}
          >
            Interested in working together?
            <br /> Visit our job offers{` `}
            <a href="https://boards.greenhouse.io/tweag">here</a>
            {` `}
            or <br />
            send a direct email to our recruiting team at{` `}
            <a href="mailto:jobs@tweag.io">jobs@tweag.io</a>.
          </Text>
        </div>
      </div>
    </Layout>
  )
}

/** @jsx jsx */
import { jsx, Grid, Text, useThemeUI } from "theme-ui"
import loadable from "@loadable/component"

import { SectionHeading } from "../../components"
import { DefaulLayout as Layout } from "../../layouts"
import { SEO } from "../../components"
// import MagicGrid from "./components/magic-grid"
import { parsePhotos, parseProfiles } from "./utils/query"

import introImage from "../../images/img18.svg"
import * as animations from "./styles/animations.module.css"
import { LoadingAnimation } from "./components/loading-animation"

const MagicGrid = loadable(() => import(`./components/magic-grid`), {
  fallback: <LoadingAnimation />,
})

const introText = `Tweag is a global team working remotely from anywhere on Earth or from hubs in Paris, Zürich, London and Utrecht. We sure love connecting science and engineering. Yet you'd be surprised to hear what else we wake up for every day. Check it out!`

const IntroductionSection = () => {
  const { theme: t } = useThemeUI()
  return (
    <div
      className="introductionSection"
      sx={{
        gap: [`15px`, `40px`],
      }}
      css={`
        display: grid;
        width: 100%;
        grid-template:
          "title"
          "lead"
          "papers" / 1fr;

        @media screen and (min-width: ${t.breakpoints[1]}) {
          grid-template:
            "title picture"
            "lead picture"
            "papers picture" / 6fr 4fr;
        }
      `}
    >
      <Text
        as="div"
        className={`${animations.slideAndFadeIn} title`}
        sx={{
          // minHeight: `100px`,
          textTransform: `uppercase`,
          fontSize: [`34px`, `66px`],
          fontWeight: 700,
          lineHeight: [1],
          mt: [`20px`, `50px`],
          gridArea: `title`,
        }}
      >
        {`Meet The People Behind Tweag`}
      </Text>
      <Text
        as="div"
        sx={{
          fontSize: [`18px`, `27px`],
          lineHeight: [1.2, `35px`],
          gridArea: `lead`,
        }}
        className="lead"
      >
        {introText}
      </Text>
      <img
        src={introImage}
        className="picture"
        css={`
          display: none;
          grid-area: picture;
          aspect-ratio: 1/1;
          height: 100%;
          justify-self: end;

          @media screen and (min-width: ${t.breakpoints[1]}) {
            display: block;
          }
        `}
      />
    </div>
  )
}

const Team = ({ data }) => {
  const photos = parsePhotos(data)
  const profiles = parseProfiles(data)
  const tags = [
    `haskell`,
    `nix`,
    `mathematics`,
    `bazel`,
    `python`,
    `functional programming`,
    `physics`,
    `devops`,
    `GHC`,
    `purescript`,
  ]

  const magicGridProps = { profiles, photos, tags }
  return (
    <Layout>
      <SEO title="Team" pathname="/team" />
      <div
        className="section s_white viewport-section transition-section"
        sx={{
          pt: [`65px`, `135px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1"
          gap={[`35px`]}
          sx={{
            px: [`15px`, `0px`],
            pl: [`15px`, `60px`, `120px`],
            pr: [`15px`, `60px`, `120px`],
            gridColumnStart: [`auto`, 1],
            gridColumnEnd: [`auto`, 4],
            gridAutoRows: [`max-content`],
            width: `100%`,
            mb: [`0px`, `3rem`],
          }}
        >
          <SectionHeading
            customSx={{
              justifySelf: `start`,
              alignSelf: `start`,
            }}
          >
            Our Team
          </SectionHeading>
          <IntroductionSection />
          {/* Content Goes Here */}
        </Grid>
        <MagicGrid {...magicGridProps} />
      </div>
    </Layout>
  )
}

export default Team

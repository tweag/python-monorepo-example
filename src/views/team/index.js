/** @jsx jsx */
import { jsx, Grid, Text } from "theme-ui"
import loadable from "@loadable/component"

import { SectionHeading } from "../../components"
import { DefaulLayout as Layout } from "../../layouts"
import { SEO } from "../../components"
// import MagicGrid from "./components/magic-grid"
import { parsePhotos, parseProfiles } from "./utils/query"

import introImage from "../../images/img18.svg"
import styles from "./styles/team.module.css"
import animations from "./styles/animations.module.css"
import { LoadingAnimation } from "./components/loading-animation"

const MagicGrid = loadable(() => import(`./components/magic-grid`), {
  fallback: <LoadingAnimation />,
})

const introText = `Tweag is a unique global team of over eighty engineers bringing tomorrow’s software techniques into today’s production systems, forming a network that connects our deep tech clients, Open Source communities and the research realm.`

const IntroductionSection = () => {
  return (
    <div className={styles.introductionSection} sx={{ gap: [`15px`, `40px`] }}>
      <Text
        className={`${animations.slideAndFadeIn} ${styles.title}`}
        sx={{
          // minHeight: `100px`,
          textTransform: `uppercase`,
          fontSize: [`34px`, `34px`, `66px`],
          fontWeight: 700,
          lineHeight: [1, 1],
          mt: [`20px`, `20px`, `50px`],
        }}
      >
        {`Meet The People Behind Tweag`}
      </Text>
      <Text
        sx={{
          fontSize: [`18px`, `18px`, `27px`],
          lineHeight: [1.2, 1.2, `35px`],
        }}
        className={styles.lead}
      >
        {introText}
      </Text>
      <img src={introImage} className={styles.picture} />
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
          pt: [`65px`, `65px`, `135px`],
        }}
      >
        <Grid
          className="transition-section__transition--slide-fade-in bottom-in only-above-1"
          gap={[`35px`, `35px`]}
          sx={{
            px: [`15px`, `15px`, `0px`],
            pl: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            pr: [`15px`, `15px`, `60px`, `60px`, `60px`, `60px`, `120px`],
            gridColumnStart: [`auto`, `auto`, 1],
            gridColumnEnd: [`auto`, `auto`, 4],
            gridAutoRows: [`max-content`],
            width: `100%`,
            mb: [`0px`, `0px`, `3rem`],
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

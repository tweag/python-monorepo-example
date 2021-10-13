/** @jsx jsx */
import { jsx, Grid, Text } from "theme-ui"

import { SectionHeading } from "../../components"
import { DefaulLayout as Layout } from "../../layouts"
import { SEO } from "../../components"

import introImage from "../../images/img18.svg"
import styles from "./styles/team.module.css"

const introText = `Tweag is a unique global team of over eighty engineers bringing tomorrow’s software techniques into today’s production systems, forming a network that connects our deep tech clients, Open Source communities and the research realm.`

const IntroductionSection = () => {
  return (
    <div className={styles.introductionSection} sx={{ gap: [`15px`, `40px`] }}>
      <Text
        className={`transition-section__transition--slide-fade-in bottom-in ${styles.title}`}
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

const Team = () => {
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
      </div>
    </Layout>
  )
}

export default Team

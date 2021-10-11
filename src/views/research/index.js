/** @jsx jsx */
import { jsx, Grid, Text } from "theme-ui"

import { SectionHeading } from "../../components"
import { DefaulLayout as Layout } from "../../layouts"
import { SEO } from "../../components"

import { Accordion } from "./components/Accordion"
import ArticleBox from "./components/article-box"
import { parseArticles } from "./utils"

import introImage from "../../images/img17.svg"
import styles from "./styles/research.module.css"

const introText = `Tweag contribute regularly to scholarly research communities, publishing papers and attending research-oriented conferences.`

const IntroductionSection = ({ papers }) => {
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
        {`Tweag's Academic Research`}
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
      <Text
        className={`transition-section__transition--slide-fade-in bottom-in ${styles.papers}`}
        sx={{
          fontSize: [`16px`, `16px`, `27px`],
          lineHeight: [1.2],
          fontWeight: [400],
        }}
      >
        <p sx={{ fontWeight: 700 }} className={styles.papers}>
          TOTAL PAPERS: {papers}
        </p>
      </Text>
      <img src={introImage} className={styles.picture} />
    </div>
  )
}

const Research = ({ data }) => {
  const articles = data.allArticlesYaml.edges.map(({ node }) => node)
  const pdfFiles = data.allFile.edges.map(({ node }) => node)

  const articleBoxProps = parseArticles(articles, pdfFiles)

  return (
    <Layout>
      <SEO title="Research" pathname="/research" />
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
            Research
          </SectionHeading>
          <IntroductionSection papers={articles.length} />
          {/* Content Goes Here */}
          <Accordion>
            <ArticleBox {...articleBoxProps} />
          </Accordion>
        </Grid>
      </div>
    </Layout>
  )
}

export default Research

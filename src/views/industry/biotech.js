/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"

import {
  TemplateFirstSection,
  TemplateExamples,
  TemplateCaseStudies,
} from "./components"

import { CallToActionFooter, Layout, SEO } from "../../components"

const content = {
  title: `BIOTECH`,
  headline: `WE PARTNER WITH VISIONARY STARTUPS AND GLOBAL LEADERS TO ADVANCE
              MEDICINE THROUGH TECHNOLOGY.`,
  paras: [
    <Fragment key={0}>
      Biotechnology deals with enormous amounts of heterogeneous data from a
      range of different sources. Tweag enables biotechnology companies to
      organize and streamline this diverse data to train powerful statistical
      models that guide meaningful discoveries and business decisions.
    </Fragment>,
    <Fragment key={1}>
      We combine software engineering with life sciences expertise &ndash; Tweag
      engineers have academic and industry backgrounds in human genetics,
      biochemistry and related fields. This gives us a deeper understanding of
      your vision and the ability to implement the most effective solution.
    </Fragment>,
  ],
  examples: [
    <Fragment key={0}>
      Maintaining a database and web portal that gathers and presents
      genotype-phenotype data for <strong>Pfizer.</strong>
    </Fragment>,
    <Fragment key={1}>
      Developing software that runs models of blood flow in the human body to
      analyze drug effects on patients with{` `}
      <strong>NovaDiscovery.</strong>
    </Fragment>,
    <Fragment key={2}>
      Creating data pipelines and a data lake that allows bioanalysts to access
      a large variety of genetic data in natural form for{` `}
      <strong>Pfizer.</strong>
    </Fragment>,
    <Fragment key={3}>
      Developing off-the-shelf statistical routines and analysis tools to
      guarantee the safety and correctness of software medical devices with{` `}
      <strong>Amgen.</strong>
    </Fragment>,
  ],
  caseStudies: {
    projects: [
      {
        client: `Pfizer`,
        description: `Making genetic data accessible for drug discovery.`,
      },
      {
        client: `NovaDiscovery`,
        description: `Simulating human physiology to cut clinical trial costs.`,
      },
    ],
    testimonals: {
      main: {
        testimonal: (
          <Fragment>
            I really value Tweag&rsquo;s rapid prototyping and their creativity
            and willingness to try different things.
          </Fragment>
        ),
        from: `Eric Fauman, Senior Scientific Director, Head Integrative Biology, Pfizer`,
      },
    },
  },
}

const BiotechPage = () => {
  const { title, headline, paras, examples, caseStudies } = content
  return (
    <Layout
      fullPageFooter
      footer={
        <div className="section viewport-section s_beige transition-section">
          <CallToActionFooter
            title={`Ready to achieve your big vision?`}
            backdropVariant={4}
            transitionClass={`transition-section__transition--slide-fade-in`}
            customWrapperSx={{
              py: [`40px`, `40px`, `60px`],
            }}
          />
        </div>
      }
    >
      <SEO title="Biotech" />
      <div
        className="section s_white"
        sx={{
          mb: [`50px`],
        }}
      >
        <TemplateFirstSection title={title} headline={headline} paras={paras} />
      </div>
      <div className="section s_beige">
        <TemplateExamples examples={examples} />
      </div>
      <div className="section s_green">
        <TemplateCaseStudies caseStudies={caseStudies} />
      </div>
    </Layout>
  )
}

export default BiotechPage

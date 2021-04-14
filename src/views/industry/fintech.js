/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"

import { DefaulLayout as Layout } from "../../layouts"
import { CallToActionFooter, SEO } from "../../components"

import {
  TemplateFirstSection,
  TemplateExamples,
  TemplateCaseStudies,
} from "./components"

const content = {
  title: `fintech`,
  headline: `We partner with visionary startups and global leaders to manage
              assets safely`,
  paras: [
    `Financial assets carry plenty of risk already. The financial
                industry has no appetite for adding risk via the systems that
                are used to manage financial assets. The industry requires tools
                and systems worthy of trust.`,
    `Tweag specializes in building high-assurance software for fintech
              companies, from pioneering blockchain startups to large global
              banks. We apply lightweight formal methods that ensure key system
              components are built to specification. We use modern software
              engineering practices that make it easier to find and eliminate
              bugs in a transparent way.`,
    `We also understand financial products. Our background in finance,
              mathematics and data science means we speak your language, and we
              can reach a deep understanding of your challenges and goals. This
              enables us to build innovative tools that make your professional
              team be more efficient and productive in their work.`,
  ],
  examples: [
    <Fragment key={0}>
      Tweagers were leaders of the design of Plutus, the smart contract language
      for one of the world&rsquo;s top-10 cryptocurrencies with{` `}
      <strong>IOHK</strong>
      {`.`}
    </Fragment>,
    <Fragment key={1}>
      We made a global bank&rsquo;s booking system for equity swaps more stable
      and performant, allowing it to scale to new use cases.
    </Fragment>,
    <Fragment key={2}>
      Tweag streamlined devops and infrastructure for{` `}
      <strong>Digital Asset</strong> so they could focus on things like
      replacing the ASX legacy clearing and settlement system with multi-party,
      automated, and simplified workflows.
    </Fragment>,
    <Fragment key={3}>
      We helped <strong>Gain Theory</strong> rethink marketing investments with
      ROI models powered by software built by Tweag.
    </Fragment>,
  ],
  caseStudies: {
    projects: [
      {
        client: `IOHK`,
        description: `Designing Plutus, the smart contract language for a world
                  top-10 currency.`,
      },
    ],
    testimonals: {
      main: {
        testimonal: `Working with Tweag has been a great honor. Their team offers
                exceptional talent and a technical skill set that we might not
                have otherwise found in our direct recruitment efforts. They
                provide our executive team with insightful guidance, and always
                makes us feel like they are part of our team.`,
        from: `Tamara Haasen, Chief of Staff, IOHK`,
      },
    },
  },
}

const FintechPage = () => {
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
      <SEO title="Fintech" />
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

export default FintechPage

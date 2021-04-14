/** @jsx jsx */
import { jsx } from "theme-ui"

import { DefaulLayout as Layout } from "../../layouts"
import { CallToActionFooter, SEO } from "../../components"
import { TemplateFirstSection } from "./components"

const content = {
  title: `Autonomous vehicles`,
  headline: `We partner with visionary startups and global leaders to build
              self-driving cars`,
  paras: [
    `Automobile safety is critical. There is no room for bugs when
                human life is at stake. Tweag builds high-assurance software
                that meets the extensive safety demands of driverless
                technology. We help clients design and deploy solutions that are
                reliable, reproducible and maintainable. Our clients are shaping
                the future of driverless technology.`,
    `Our approach to safety comes down to science. We apply lightweight
              formal methods to create testing and analysis tools that find bugs
              early.`,
    `Our capabilities go beyond safety. Tweag helps you quickly scale
              your engineering performance to build and innovate faster. Our
              expertise in infrastructure and software development processes
              enable you to resolve bottlenecks and increase developer
              productivity and output. Safer and faster development is not a
              choice you have to make.`,
  ],
}

const AutonomousPage = () => {
  const { title, headline, paras } = content
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
      <SEO title="Autonomous vehicles" />
      <div
        className="section s_white"
        sx={{
          mb: [`50px`],
        }}
      >
        <TemplateFirstSection title={title} headline={headline} paras={paras} />
      </div>
    </Layout>
  )
}

export default AutonomousPage

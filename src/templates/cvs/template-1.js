/** @jsx jsx */
import { jsx, Grid, Text, Box, Image } from "theme-ui"
import { graphql } from "gatsby"
import { micromark } from "micromark"

import { CV } from "../../layouts"
import { TitleName, ListFeature, H1, H5 } from "./view"

import blackPattern from "../../images/pattern-black-1.svg"
import tweaglogo from "../../images/logo_tweag_black_cv_template.svg"

const TemplateCV1 = ({ data }) => {
  const {
    name,
    pronouns,
    github,
    shortDescription,
    bio,
    skills,
    speaks,
    publications,
    experience,
    education,
  } = data.profile

  return (
    <CV>
      <Box
        className="s_yellow"
        sx={{
          width: [`100%`],
          maxWidth: [`100%`, `620px`, `830px`],
          p: [`15px`],
          margin: `auto`,
        }}
      >
        <div
          sx={{
            columnCount: [1, 2],
            columnGap: `2rem`,
          }}
        >
          <TitleName fullname={name} pronouns={pronouns} github={github} />
          <Grid columns={2}>
            <ListFeature title={`Key Skills`} features={skills || []} />
            <ListFeature title={`Languages`} features={speaks || []} />
          </Grid>
          <Grid sx={{ gridAutoRows: `max-content` }}>
            <H1>bio</H1>
            <Text
              as="p"
              sx={{ fontSize: `13px`, lineHeight: 1.3, breakInside: `avoid` }}
            >
              {shortDescription}
            </Text>
            <Text
              as="p"
              sx={{ fontSize: `13px`, lineHeight: 1.3 }}
              dangerouslySetInnerHTML={{ __html: micromark(bio) }}
            />
          </Grid>
          <Grid sx={{ gridAutoRows: `max-content` }}>
            {experience && experience.length && <H1>experience</H1>}
            {experience &&
              experience.length &&
              experience.map(({ employer, role, years, description }, i) => (
                <div
                  key={i}
                  gap={`2px`}
                  css={`
                    display: inline-block;
                  `}
                >
                  <H5>
                    {employer} &#8212; {role}
                  </H5>
                  <H5
                    customSx={{
                      fontWeight: `normal`,
                    }}
                  >
                    {years}
                  </H5>
                  {(description || []).map((desc, i) => (
                    <Text
                      as="div"
                      key={i}
                      sx={{
                        fontSize: `13px`,
                        lineHeight: 1.3,
                        fontWeight: `normal`,
                      }}
                      dangerouslySetInnerHTML={{ __html: micromark(desc) }}
                    />
                  ))}
                </div>
              ))}
          </Grid>
          <Grid
            sx={{
              gridRow: [6, 3],
            }}
          >
            {publications && publications.length && (
              <Grid sx={{ gridAutoRows: `max-content` }}>
                <H1>Key Publications</H1>
                <Grid>
                  {publications.map((publication, index) => (
                    <Text
                      as="div"
                      key={index}
                      sx={{
                        fontSize: `10px`,
                        lineHeight: 1.3,
                      }}
                    >
                      [{index + 1}]{` `}
                      {publication.link ? (
                        <a href={publication.link}>{publication.description}</a>
                      ) : (
                        publication.description
                      )}
                    </Text>
                  ))}
                </Grid>
              </Grid>
            )}
          </Grid>
          <Grid sx={{ gridRow: [5, 3], pb: [0, `180px`] }}>
            {education && education.length && (
              <Grid sx={{ gridAutoRows: `max-content` }}>
                <H1>Education</H1>
                {education.map(
                  (
                    { qualification, name, institution, years, description },
                    index
                  ) => (
                    <Grid key={index} gap={`2px`}>
                      <H5>
                        {institution} &#8212; {qualification} &#8212; {name}
                      </H5>
                      <H5>{years}</H5>
                      {(description || []).map((desc, i) => (
                        <Text
                          as="div"
                          key={i}
                          sx={{
                            fontSize: `13px`,
                            lineHeight: 1.3,
                            fontWeight: `normal`,
                          }}
                          dangerouslySetInnerHTML={{ __html: micromark(desc) }}
                        />
                      ))}
                    </Grid>
                  )
                )}
              </Grid>
            )}
          </Grid>
        </div>
        <div
          sx={{
            marginTop: `1rem`,
            display: `flex`,
            width: `100%`,
            alignItems: `end`,
            justifyContent: `space-between`,
          }}
        >
          <Image src={tweaglogo} alt="tweag logo" />
          <Image
            src={blackPattern}
            sx={{
              display: [`none`, `block`],
            }}
          />
        </div>
      </Box>
    </CV>
  )
}

export default TemplateCV1

export const pageQuery = graphql`
  query ProfileBySlug($slug: String!) {
    profile: profilesYaml(slug: { eq: $slug }) {
      slug
      name
      pronouns
      github
      shortDescription
      bio
      skills
      speaks
      publications {
        description
        link
      }
      experience {
        employer
        role
        years
        description
      }
      education {
        qualification
        name
        institution
        years
        description
      }
    }
  }
`

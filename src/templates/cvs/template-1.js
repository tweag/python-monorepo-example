/** @jsx jsx */
import { jsx, Grid, Text, Box, Image } from "theme-ui"
import { graphql } from "gatsby"

import { CV } from "../../layouts"

import blackPattern from "../../images/pattern-black-1.svg"
import tweaglogo from "../../images/logo_tweag_black_cv_template.svg"

const H1 = ({ children }) => {
  return (
    <Text
      sx={{
        textTransform: `uppercase`,
        fontSize: [`42px`],
        fontWeight: 700,
        lineHeight: 1,
      }}
    >
      {children}
    </Text>
  )
}

const H2 = ({ children }) => {
  return (
    <Text
      sx={{
        fontSize: [`24px`],
        fontWeight: 700,
        textTransform: `uppercase`,
        lineHeight: 1,
      }}
    >
      {children}
    </Text>
  )
}

const H5 = ({ children, customSx, className }) => {
  return (
    <Text
      className={className}
      sx={{
        textTransform: `uppercase`,
        fontSize: `13px`,
        fontWeight: 700,
        lineHeight: 1,
        ...customSx,
      }}
    >
      {children}
    </Text>
  )
}

const TitleName = ({ fullname, pronouns }) => {
  return (
    <Grid
      sx={{
        gridAutoRows: `max-content`,
      }}
      gap={`0px`}
    >
      <H1>{fullname}</H1>
      <H2>TWEAGER</H2>
      <H5 customSx={{ lineHeight: 1.6 }}>{pronouns}</H5>
    </Grid>
  )
}

const ListFeature = ({ title, features }) => {
  return (
    <Grid gap={`10px`} sx={{ gridAutoRows: `max-content` }}>
      <H2>{title}</H2>
      <Grid
        gap={0}
        sx={{
          gridAutoRows: `max-content`,
        }}
      >
        {features.map(feature => (
          <Text
            key={feature}
            sx={{
              fontSize: [`13px`],
              fontWeight: `normal`,
              lineHeight: [1.1],
            }}
          >
            {feature}
          </Text>
        ))}
      </Grid>
    </Grid>
  )
}

const TemplateCV1 = ({ data }) => {
  const {
    name,
    pronouns,
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
        <Grid
          columns={[1, 1, 2]}
          sx={{
            rowGap: [`40px`],
            columnGap: [`60px`],
            gridAutoRows: `max-content`,
          }}
        >
          <TitleName fullname={name} pronouns={pronouns} />
          <Grid columns={2}>
            <ListFeature title={`Key Skills`} features={skills || []} />
            <ListFeature title={`Languages`} features={speaks || []} />
          </Grid>
          <Grid sx={{ gridAutoRows: `max-content` }}>
            <H1>bio</H1>
            <Text as="p" sx={{ fontSize: `13px`, lineHeight: 1.3 }}>
              {shortDescription}
            </Text>
            <Text as="p" sx={{ fontSize: `13px`, lineHeight: 1.3 }}>
              {bio}
            </Text>
          </Grid>
          <Grid sx={{ gridAutoRows: `max-content` }}>
            {experience && experience.length && <H1>experience</H1>}
            {experience &&
              experience.length &&
              experience.map(({ employer, role, years, description }, i) => (
                <Grid key={i} gap={`2px`}>
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
                      key={i}
                      sx={{
                        fontSize: `13px`,
                        lineHeight: 1.3,
                        fontWeight: `normal`,
                      }}
                    >
                      {desc}
                    </Text>
                  ))}
                </Grid>
              ))}
          </Grid>
          <Grid
            sx={{
              gridRow: [6, 6, 3],
            }}
          >
            {publications && publications.length && (
              <Grid sx={{ gridAutoRows: `max-content` }}>
                <H1>Key Publications</H1>
                <Grid>
                  {publications.map((publication, index) => (
                    <Text
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
          <Grid sx={{ gridRow: [5, 5, 3], pb: [0, 0, `180px`] }}>
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
                          key={i}
                          sx={{
                            fontSize: `13px`,
                            lineHeight: 1.3,
                            fontWeight: `normal`,
                          }}
                        >
                          {desc}
                        </Text>
                      ))}
                    </Grid>
                  )
                )}
              </Grid>
            )}
          </Grid>
          <Image src={tweaglogo} alt="tweag logo" />
          <Image
            src={blackPattern}
            sx={{
              display: [`none`, `none`, `block`],
              alignSelf: `end`,
              justifySelf: `end`,
              mr: [0, 0, `-15px`],
              mb: [0, 0, `-15px`],
              mt: [0, 0, `-150px`],
            }}
          />
        </Grid>
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

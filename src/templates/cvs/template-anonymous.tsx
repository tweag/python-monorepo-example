/** @jsx jsx */

import { graphql } from "gatsby"
import { micromark } from "micromark"
import React, { useEffect, useState } from "react"
import { Box, Grid, Image, jsx, Text } from "theme-ui"
import { CV } from "../../layouts"
import { H1, H5, ListFeature, TitleName } from "./view"
import tweaglogo from "../../images/logo_tweag_black_cv_template.svg"
import blackPattern from "../../images/pattern-black-1.svg"
import { css } from "@emotion/react"

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

type Profile = {
  slug: string
  name: string
  pronouns: string
  github: string
  shortDescription: string
  bio: string
  skills: string[]
  speaks: string[]
  publications: {
    description: string
    link: string
  }[]
  experience: {
    employer: string
    role: string
    years: string
    description: string[]
  }[]
  education: {
    qualification: string
    name: string
    institution: string
    years: string
    description: string[]
  }[]
}

type Props = {
  data: {
    profile: Profile
  }
}

const TemplateAnonymous: React.FC<Props> = ({ data }) => {
  const { profile } = data
  const [title, setTitle] = useState(`Loading...`)

  useEffect(() => {
    const queryParams = new URLSearchParams(window.location.search)
    const titleParam = queryParams.get(`t`)
    setTitle(titleParam ? atob(titleParam) : `Functional Engineer`)
  })

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
          <TitleName fullname={title} isAnonymous={true} />
          <Grid columns={2}>
            <ListFeature title={`Key Skills`} features={profile.skills || []} />
            <ListFeature title={`Languages`} features={profile.speaks || []} />
          </Grid>
          <Grid sx={{ gridAutoRows: `max-content` }}>
            <H1>bio</H1>
            <Text
              as="p"
              sx={{ fontSize: `13px`, lineHeight: 1.3, breakInside: `avoid` }}
            >
              {profile.shortDescription}
            </Text>
            <Text
              as="p"
              sx={{ fontSize: `13px`, lineHeight: 1.3 }}
              dangerouslySetInnerHTML={{ __html: micromark(profile.bio) }}
            />
          </Grid>
          <Grid sx={{ gridAutoRows: `max-content` }}>
            {profile.experience && profile.experience.length && (
              <H1>experience</H1>
            )}
            {profile.experience &&
              profile.experience.length &&
              profile.experience.map(
                ({ employer, role, years, description }, i) => (
                  <div
                    key={i}
                    css={css`
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
                )
              )}
          </Grid>
          <Grid
            sx={{
              gridRow: [6, 3],
            }}
          >
            {profile.publications && profile.publications.length && (
              <Grid sx={{ gridAutoRows: `max-content` }}>
                <H1>Key Publications</H1>
                <Grid>
                  {profile.publications.map((publication, index) => (
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
            {profile.education && profile.education.length && (
              <Grid sx={{ gridAutoRows: `max-content` }}>
                <H1>Education</H1>
                {profile.education.map(
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

export default TemplateAnonymous

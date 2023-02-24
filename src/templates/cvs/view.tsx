/** @jsx jsx */

import { Grid, jsx, Text, ThemeUIStyleObject } from "theme-ui"

type HProps = {
  children: React.ReactNode
  customSx?: ThemeUIStyleObject
  className?: string
}

export const H1: React.FC<HProps> = ({ children }) => {
  return (
    <Text
      as="div"
      sx={{
        textTransform: `uppercase`,
        fontSize: [`42px`],
        fontWeight: 700,
        lineHeight: 1,
        marginTop: `2rem`,
      }}
    >
      {children}
    </Text>
  )
}

export const H2: React.FC<HProps> = ({ children }) => {
  return (
    <Text
      as="div"
      sx={{
        fontSize: [`24px`],
        fontWeight: 700,
        textTransform: `uppercase`,
        lineHeight: 1,
        marginTop: `1rem`,
      }}
    >
      {children}
    </Text>
  )
}

export const H5: React.FC<HProps> = ({ children, customSx, className }) => {
  return (
    <Text
      as="div"
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

type TitleNameProps = {
  fullname: string
  pronouns?: string
  github?: string
  isAnonymous?: boolean
}

export const TitleName: React.FC<TitleNameProps> = ({
  fullname,
  pronouns,
  github,
  isAnonymous = false,
}) => {
  const tweagerInfo = !isAnonymous ? (
    <div>
      <H2>TWEAGER</H2>
      {pronouns ? <H5 customSx={{ lineHeight: 1.6 }}>{pronouns}</H5> : ``}
      {github ? (
        <H5 customSx={{ lineHeight: 1.6 }}>
          <a href={`https://github.com/` + github}>GitHub</a>
        </H5>
      ) : (
        ``
      )}
    </div>
  ) : (
    ``
  )

  return (
    <Grid
      sx={{
        gridAutoRows: `max-content`,
      }}
      gap={`0px`}
    >
      <div
        sx={{
          textTransform: `uppercase`,
          fontSize: [`42px`],
          fontWeight: 700,
          lineHeight: 1,
        }}
      >
        {fullname}
      </div>
      {tweagerInfo}
    </Grid>
  )
}

type ListFeatureProps = {
  title: string
  features: string[]
}

export const ListFeature: React.FC<ListFeatureProps> = ({
  title,
  features,
}) => {
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
            as="div"
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

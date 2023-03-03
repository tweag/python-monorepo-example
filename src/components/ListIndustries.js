/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Box, Text } from "theme-ui"
import { Link } from "gatsby"

function ListIndustries({ industries, customWrapperSx, transitionClass }) {
  return (
    <Grid
      columns={[1]}
      sx={{
        px: [`15px`, `100px`],
        py: [`40px`],
        textAlign: [`start`, `center`],
        ...customWrapperSx,
      }}
      gap={[`40px`]}
    >
      <Box className={`${transitionClass} bottom-in  only-above-1`}>
        <Text
          as="div"
          sx={{
            fontSize: [`24px`, `34px`, `42px`],
            lineHeight: [1],
            fontWeight: [700],
            textTransform: `uppercase`,
          }}
        >
          KEY INDUSTRIES WE SERVE
        </Text>
      </Box>
      <Grid gap={[`40px`]} columns={[1, 3]}>
        {industries.map(({ h, link, p, src }, i) => (
          <Grid
            key={i}
            sx={{
              transitionDelay: `${0.5 + 0.1 * i}s`,
            }}
            className={`${transitionClass} bottom-in  only-above-1`}
          >
            <Box>
              <img
                sx={{
                  width: [`50%`, `100%`, `50%`],
                }}
                src={src}
                alt=""
              />
            </Box>
            <Text
              as="div"
              sx={{
                fontSize: [`18px`, `27px`, `34px`],
                lineHeight: [1.1],
                fontWeight: [700],
              }}
            >
              {h}
            </Text>
            <Text
              as="p"
              sx={{
                fontSize: [`18px`],
                lineHeight: [`26px`],
              }}
            >
              {p}
            </Text>
            <Link
              sx={{
                justifySelf: [`start`, `center`],
                alignSelf: [`center`],
              }}
              to={link}
              className="button button-secondary button-medium min-5__button-large pre-arrow-right"
            >
              Learn more
            </Link>
          </Grid>
        ))}
      </Grid>
    </Grid>
  )
}

export default ListIndustries
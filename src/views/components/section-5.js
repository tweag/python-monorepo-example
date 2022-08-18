/** @jsx jsx */
import { jsx } from "theme-ui"
import { Grid, Box, Text } from "theme-ui"

import logoAmgen from "../../images/logo_amgen.png"
import logoCea from "../../images/logo_cea.png"
import logoGoogle from "../../images/logo_google.png"
import logoPfizer from "../../images/logo_pfizer.png"
import logoTarget from "../../images/logo_target.png"
import logoOrange from "../../images/logo_orange.png"

const partners = [
  {
    src: logoAmgen,
    name: `Amgen`,
  },
  {
    src: logoGoogle,
    name: `Google`,
  },
  {
    src: logoCea,
    name: `CEA`,
  },
  {
    src: logoOrange,
    name: `Orange`,
  },
  {
    src: logoPfizer,
    name: `Pfizer`,
  },
  {
    src: logoTarget,
    name: `Target`,
  },
]

function Section5() {
  return (
    <Grid
      columns={[1]}
      gap={[`30px`, `60px`]}
      sx={{
        px: [`15px`, `15px`, `100px`],
        maxWidth: [`1440px`, `1440px`, `100%`],
        margin: `auto`,
      }}
    >
      <Box className="transition--slide-fade-in bottom-in  only-above-1">
        <Text
          as="div"
          sx={{
            textAlign: [`left`, `center`],
            fontSize: [`24px`, `34px`, `42px`],
            lineHeight: [1],
            fontWeight: 700,
            textTransform: `uppercase`,
          }}
        >
          Trusted by leaders <br />
          &amp; innovators
        </Text>
      </Box>
      <Grid columns={[2]} gap={[`20px`, `60px`]}>
        {partners.map(({ src, name }, i) => (
          <Box
            key={name}
            sx={{
              display: `flex`,
              justifyContent: `center`,
              alignItems: `center`,
            }}
            className={`transition--slide-fade-in bottom-in  only-above-1 delayed-${
              5 + 1 * i
            }`}
          >
            <img
              sx={{
                maxWidth: [`70%`],
                width: [`100px`, `100px`, `150px`],
              }}
              src={src}
              alt={name}
            />
          </Box>
        ))}
      </Grid>
    </Grid>
  )
}

export default Section5

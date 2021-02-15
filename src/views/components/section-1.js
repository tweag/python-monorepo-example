/** @jsx jsx */
import { jsx } from "theme-ui"
import React from "react"
import { Grid, Box } from "theme-ui"

import homeAnim from "../../images/home.svg"

import { Arrow } from "../../components"

export default function Section1() {
  return (
    <React.Fragment>
      <Grid
        columns={[1, 2]}
        sx={{
          margin: [`auto`, `auto`, `0 15%`],
          textAlign: [`center`, `start`],
          maxWidth: [`100%`, `740px`, `100%`],
          py: [`50px`, 0],
        }}
        gap={[`50px`, 5, `10%`]}
      >
        <Box className={`transition--slide-fade-in bottom-in only-above-1`}>
          <img
            width="600"
            height="600"
            src={homeAnim}
            sx={{
              width: `100%`,
              height: `auto`,
              maxWidth: [`280px`, `300px`, `90%`],
            }}
          />
        </Box>
        <Box
          sx={{
            px: [`15px`],
            fontSize: [`16px`, `27px`],
          }}
          className="transition--slide-fade-in bottom-in delayed only-above-1"
        >
          <h1
            className="hide-in-percy"
            sx={{
              mb: [`10px`],
              minHeight: [null, `200px`],
              minWidth: [null, `400px`],
            }}
          >
            <div>
              SCALE YOUR <br />
              ENGINEERING <br />
              POWER.
            </div>
            {/* TODO: react-typed is dead and needs replacement.
            <Typed
              strings={[`SCALE YOUR <br/>ENGINEERING <br/>POWER.`]}
              typeSpeed={50}
              showCursor={false}
            /> */}
          </h1>
          <p>
            We enable deep-tech startups to achieve their vision, from research
            to product delivery.
          </p>
        </Box>
      </Grid>
      <Arrow
        level={1}
        size={`40px`}
        direction={`down`}
        customSx={{
          position: `relative`,
          top: [0, 0, `10%`],
          margin: [`0 auto`],
          display: [`none`, `none`, `block`],
        }}
      />
    </React.Fragment>
  )
}

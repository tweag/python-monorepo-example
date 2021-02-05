/** @jsx jsx */
import { jsx } from "theme-ui"
import React from "react"
import Typed from "react-typed"
import { Grid, Box } from "theme-ui"

export default function Section1({ homePreview, homeVideo }) {
  return (
    <React.Fragment>
      <Grid
        columns={[1, 1, 2]}
        sx={{
          margin: [
            `auto`,
            `auto`,
            `auto`,
            `0 10%`,
            `0 10%`,
            `0 10%`,
            `0 10%`,
            `0 15%`,
          ],
          textAlign: [`center`, `center`, `start`],
          maxWidth: [`100%`, `100%`, `740px`, `100%`],
        }}
        gap={[`50px`, `50px`, 5, `10%`]}
      >
        <Box className={`transition--slide-fade-in bottom-in only-above-1`}>
          <video
            width="600"
            height="600"
            loop
            autoPlay
            muted
            playsInline
            data-keepplaying
            poster={homePreview}
            sx={{
              width: `100%`,
              height: `auto`,
              maxWidth: [
                `280px`,
                `280px`,
                `300px`,
                `380px`,
                `450px`,
                `450px`,
                `90%`,
              ],
            }}
          >
            <source src={homeVideo} type="video/mp4" />
          </video>
        </Box>
        <Box
          sx={{
            px: [`15px`, `15px`],
            fontSize: [`16px`, `16px`, `27px`],
          }}
          className="transition--slide-fade-in bottom-in delayed only-above-1"
        >
          <h1
            className="typeit hide-in-percy"
            sx={{
              mb: [`10px`],
            }}
          >
            <Typed
              strings={[`SCALE YOUR ENGINEERING POWER.`]}
              typeSpeed={50}
              showCursor={false}
            />
          </h1>
          <p>
            We enable deep-tech startups to achieve their vision, from research
            to product delivery.
          </p>
        </Box>
      </Grid>
      <div
        sx={{
          position: `relative`,
          top: [0, 0, 0, `10%`],
        }}
        className="line-arrow down"
      ></div>
    </React.Fragment>
  )
}

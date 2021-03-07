/** @jsx jsx */
import { jsx } from "theme-ui"
import { Fragment } from "react"
import { Link } from "gatsby"
import { Flex, Box, Text } from "theme-ui"

import nixosImg from "../../../images/nixos-black.png"
import haskellImg from "../../../images/haskell-black.png"
import bazelImg from "../../../images/bazel-black.png"

function Community({ title, bgColor, children, bgImage, description }) {
  return (
    <Flex
      className={`section s_${bgColor} viewport-section transition-section`}
      sx={{
        flexWrap: `wrap`,
        pt: [`40px`, `40px`, `70px`],
        pb: [`40px`, `40px`],
        px: [`15px`, `15px`, `5%`],
      }}
    >
      <Text
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
        sx={{
          flexBasis: [`100%`, `100%`, `30%`],
          fontSize: [`34px`, `34px`, `66px`],
          fontWeight: [700],
          lineHeight: [1],
          minHeight: [`100px`],
          mb: [`20px`],
          textTransform: `uppercase`,
        }}
      >
        {title}
      </Text>
      <Box
        className="transition-section__transition--slide-fade-in bottom-in only-above-1"
        sx={{
          flexBasis: [`100%`, `100%`, `65%`, `65%`, `65%`, `65%`, `50%`],
          fontSize: [`18px`, `18px`, `27px`],
          lineHeight: [1, 1, 1.2],
          pl: [`auto`, `auto`, `100px`],
        }}
      >
        {description}
      </Box>
      <img
        src={bgImage}
        style={{
          position: `absolute`,
          top: `10px`,
          left: `10px`,
          width: `300px`,
          opacity: 0.1,
        }}
      />
    </Flex>
  )
}

export default function Communities() {
  return (
    <Box
      sx={{
        pt: [`75px`, `75px`, `0px`],
      }}
    >
      {[
        {
          title: `Nix`,
          bg: `red`,
          bgImg: nixosImg,
          description: (
            <Fragment>
              <p>
                Many thought leaders of the{` `}
                <a
                  className="button button-link button-lined"
                  href="https://nixos.org"
                >
                  Nix community
                </a>
                {` `}
                are Tweagers. Together with our clients, we contribute much of
                the technical roadmap, from better reproducibily and performance
                to new use cases and developer tools.
              </p>
            </Fragment>
          ),
        },
        {
          title: `Haskell`,
          bg: `orange`,
          bgImg: haskellImg,
          description: (
            <Fragment>
              <p>
                Tweagers are among the top contributors to GHC, a mature,
                state-of-the-art compiler for Haskell. Haskell is now recognized
                as the shortest path to correct, scalable code for industries
                where mistakes matter. We created language interop with{` `}
                <a
                  className="button button-link button-lined"
                  href="https://github.com/tweag/inline-java"
                >
                  Java
                </a>
                {`, `}
                <a
                  className="button button-link button-lined"
                  href="https://github.com/tweag/inline-js"
                >
                  JavaScript
                </a>
                {`, `}
                <a
                  className="button button-link button-lined"
                  href="https://github.com/fpco/inline-c"
                >
                  C
                </a>
                {` `}
                and{` `}
                <a
                  className="button button-link button-lined"
                  href="https://github.com/tweag/HaskellR"
                >
                  R
                </a>
                {`, `}
                we make Haskell{` `}
                <a
                  className="button button-link button-lined"
                  href="https://github.com/tweag/asterius"
                >
                  run in your browser
                </a>
                {`, `}
                and do the{` `}
                <a
                  className="button button-link button-lined"
                  href="https://github.com/tweag/funflow"
                >
                  heavy lifting
                </a>
                {` `}
                in your{` `}
                <a
                  className="button button-link button-lined"
                  href="https://github.com/tweag/porcupine"
                >
                  analytics pipelines
                </a>
                {`.`}
              </p>
            </Fragment>
          ),
        },
        {
          title: `Bazel`,
          bg: `grey`,
          bgImg: bazelImg,
          description: (
            <Fragment>
              <p>
                We were among the first outside of Google to adopt Bazel. We are
                {` `}
                <a
                  className="button button-link button-lined"
                  href="https://bazel.build/experts.html"
                >
                  recognized community experts
                </a>
                {`. `}
                We have contributed key features and new programming language
                support to Bazel. We did this for ourselves and now{` `}
                <Link to="/bazel" className="button button-link button-lined">
                  we can do it for you
                </Link>
                {`. `}
              </p>
            </Fragment>
          ),
        },
      ].map(({ title, description, bg, bgImg }) => (
        <Community
          key={title}
          title={title}
          bgColor={bg}
          bgImage={bgImg}
          description={description}
        />
      ))}
    </Box>
  )
}

/*
 * Copied from https://github.com/nullhook/gatsby-remark-video
 * Modified to add a span tag surrounding the video to fit the Tweag blog.
 * Modified to have defaults.
 */
const visit = require(`unist-util-visit`)
const { DEFAULT_OPTIONS } = require(`./constants`)

const matchRegExp = new RegExp(
  // Look for a "video" and then possibly ':' and then a space
  `video:?\\s` +
    // Then, optionally find "title" and then possible :
    `(?:title:?\\s"(` +
    // '.*?(?!\\").' is a trick to negative lookbehind (since negative look-behinds have poor node support for now)
    // It allows us to be a able to get "any \"escaped\" quoted value" but lazily to avoid grabbing too much
    `.*?(?!\\\\").)"` +
    // The : is optional much like with 'video', but the title itself is also optional entirely
    `:?\\s)?` +
    // Then grab the video path from the string
    `(.*)`,
  // Make it insensitive
  `i`
)

const addVideo = ({ markdownAST }, pluginOptions) => {
  visit(markdownAST, `inlineCode`, node => {
    const { value } = node
    const matches = value.match(matchRegExp)

    if (matches) {
      const title = matches[1] // May be null
      const url = matches[2].trim()
      const options = { ...DEFAULT_OPTIONS, ...pluginOptions }
      if (typeof options.spanStyleMaxWidth === `number`) {
        options.spanStyleMaxWidth += `px`
      }

      node.type = `html`
      node.value = renderVideoTag(url, {
        ...options,
        title: title || url,
      })
    }
  })
}

const renderVideoTag = (url, options) => {
  const videoNode = `
    <span
      style="
        position: ${options.spanStylePosition};
        display: ${options.spanStyleDisplay};
        margin-left: ${options.spanStyleMarginLeft};
        margin-right: ${options.spanStyleMarginRight};
        max-width: ${options.spanStyleMaxWidth};
        "
    >
      <video
        src=${url}
        width="${options.width}"
        height="${options.height}"
        preload="${options.preload}"
        title="${options.title}"
        ${options.autoplay ? `autoplay` : ``}
        ${options.muted ? `muted` : ``}
        ${options.playsinline ? `playsinline` : ``}
        ${options.controls ? `controls` : ``}
        ${options.loop ? `loop` : ``}
      ></video>
    </span>
  `

  return videoNode
}

module.exports = addVideo

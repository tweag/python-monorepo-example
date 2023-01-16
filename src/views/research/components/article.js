/** @jsx jsx */
// eslint-disable-next-line no-unused-vars
import React from "react"
import { jsx, Text, Box } from "theme-ui"
import { AccordionItem } from "./Accordion"

import * as styles from "../styles/article.module.css"

/**
 * @param {{tweag: boolean, name: string, ref?: string}[]} authors
 * @returns {JSX.Element[]}
 */
function parseAuthors(authors) {
  const result = authors.map((author, index) => {
    if (author.tweag) {
      return (
        <span key={index} sx={{ color: `#4D22A8` }}>{`${author.name}${
          index < authors.length - 1 ? `, ` : ``
        }`}</span>
      )
    } else {
      return (
        <span key={index}>
          {`${author.name}${index < authors.length - 1 ? `, ` : ``}`}
        </span>
      )
    }
  })

  return result
}

const ArticleButton = ({ url, name, key }) => {
  if (url.length < 1) {
    return <></>
  }
  return (
    <a
      className={styles.articleButton}
      target="_blank"
      rel="noreferrer"
      href={url}
      key={key}
    >
      {name}
    </a>
  )
}

/**
 * @param {{
 *    title: string,
 *    authors: { tweag: boolean, name: string}
 *    date: Date,
 *    status: string,
 *    tags: Set<string>,
 *    links: Array<[string, string]>,
 *    topString?: string
 *  }} props
 */
const Article = ({
  title,
  authors,
  status,
  links,
  pdf,
  abstract,
  topString,
}) => {
  const visiblePart = (
    <Box className={styles.visiblePartContainer}>
      {topString ? (
        <span className={styles.articleTopString}>{topString}</span>
      ) : (
        <></>
      )}
      <Box sx={{ mb: `0.8rem` }}>
        <Text as="div" className={styles.articleTitle}>
          {title}
        </Text>
        <Text as="div" sx={{ mb: `0.8rem` }}>
          {status}
        </Text>
        <Text as="div">{parseAuthors(authors)}</Text>
      </Box>
    </Box>
  )

  const invisiblePart = (
    <Box>
      <Text as="div" sx={{ mt: `2rem` }}>
        {abstract}
      </Text>
      <div className={styles.buttonBox}>
        {(links ?? []).map(([name, url]) => (
          <ArticleButton url={url} name={name} key={name + url} />
        ))}
      </div>
    </Box>
  )

  return (
    <AccordionItem
      visibleContent={visiblePart}
      invisibleContent={invisiblePart}
      colorOnExpand={true}
      className={styles.article}
    />
  )
}

export default Article

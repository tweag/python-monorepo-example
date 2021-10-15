/** @jsx jsx */
// eslint-disable-next-line no-unused-vars
import React from "react"
import { jsx, Text, Box } from "theme-ui"
import { AccordionItem } from "./Accordion"

import styles from "../styles/article.module.css"

/**
 * @param {{tweag: boolean, name: string, ref?: string}[]} authors
 * @returns {JSX.Element[]}
 */
function parseAuthors(authors) {
  const result = authors.map((author, index) => {
    if (author.tweag) {
      return (
        <span sx={{ color: `#4D22A8` }}>{`${author.name}${
          index < authors.length - 1 ? `, ` : ``
        }`}</span>
      )
    } else {
      return (
        <span>{`${author.name}${index < authors.length - 1 ? `, ` : ``}`}</span>
      )
    }
  })

  return result
}

const ArticleButton = ({ url, name, key }) => {
  if (url.length < 1) {
    console.log(`Absent PDF for this article`)
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
  key,
  topString,
}) => {
  console.log(`Info reveived: ${JSON.stringify({ links, pdf })}`)
  const visiblePart = (
    <Box className={styles.visiblePartContainer}>
      {topString ? (
        <span className={styles.articleTopString}>{topString}</span>
      ) : (
        <></>
      )}
      <Box sx={{ mb: `0.8rem` }}>
        <Text className={styles.articleTitle}>{title}</Text>
        <Text sx={{ mb: `0.8rem` }}>{status}</Text>
        <Text>{parseAuthors(authors)}</Text>
      </Box>
    </Box>
  )

  const invisiblePart = (
    <Box>
      <Text sx={{ mt: `2rem` }}>{abstract}</Text>
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
      key={key}
    />
  )
}

export default Article

/** @jsx jsx */
import { jsx } from "theme-ui"

import Article from "../components/article"
import { getFullMonth } from "../utils"

import styles from "../styles/yearly-layout.module.css"

/**
 * @param {{
 *  articles: {
 *    title: string,
 *    authors: { tweag: boolean, name: string, ref?: string }
 *    date: Date,
 *    status: string,
 *    tags: Set<string>,
 *    link: string,
 *    pdf: string,
 *  }[],
 *  year: string,
 *  month: string,
 *  key: string | number
 * }} props
 */
export const Month = ({ articles, month, year, key }) => {
  const finalArticles = []

  for (let i = 0; i < articles.length; i++) {
    finalArticles.push(
      <Article
        {...articles[i]}
        key={articles[i].title}
        topString={i === 0 ? `${getFullMonth(month)} ${year}` : false}
      />
    )
  }

  return <div key={key}>{finalArticles}</div>
}

/**
 * @param {{
 *  articles: {
 *    title: string,
 *    authors: { tweag: boolean, name: string}
 *    date: Date,
 *    status: string,
 *    tags: Set<string>,
 *    links: Array<[string, string]>,
 *  }[],
 *  year: string,
 *  key: string | number
 * }} props
 */
export const Year = ({ articles, year, key }) => {
  return (
    <div className={styles.yearBox} key={key}>
      <div className={styles.yearString}>{year}</div>
      <div className={styles.articlesByYear}>
        {articles.map(article => (
          <Article {...article} key={article.title} />
        ))}
      </div>
    </div>
  )
}

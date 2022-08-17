/** @jsx jsx */
import { jsx, useThemeUI } from "theme-ui"

import Article from "../components/article"
import { getFullMonth } from "../utils"

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
  const { theme: t } = useThemeUI()
  return (
    <div
      className="yearBox"
      key={key}
      css={`
        display: grid;
        gap: 1rem;
        grid-template: "year articles" / 1fr 6fr;

        @media screen and (max-width: ${t.breakpoints[1]}) {
          grid-template: "year" "articles" / 1fr;
        }
      `}
    >
      <div
        className="yearString"
        sx={{
          gridArea: `year`,
          fontSize: `4rem`,
          fontWeight: `1000`,
        }}
      >
        {year}
      </div>
      <div
        className="articlesByYear"
        sx={{
          gridArea: `articles`,
        }}
      >
        {articles.map(article => (
          <Article {...article} key={article.title} />
        ))}
      </div>
    </div>
  )
}

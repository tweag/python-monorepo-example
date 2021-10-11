/** @jsx jsx */
import { jsx } from "theme-ui"
import { useState, useRef, useReducer } from "react"

import {
  useTagToggledEventMonitor,
  useFirstArticleToggler,
} from "../hooks/article-box-hooks"
import { countTags, classifyArray } from "../utils"
import { Year } from "./yearly-layout"
import styles from "../styles/article-box.module.css"

/**
 * @param {string} tagName
 * @param {HTMLElement} element
 */
function dipatchTagEvent(tagName, element, state) {
  const eventName = `toggled-tag`

  const event = new Event(eventName, { bubbles: true })
  event.tagName = tagName
  event.state = state

  element.dispatchEvent(event)
}

const TagButton = ({ tagName, articles, key }) => {
  const [active, setState] = useState(false)

  const clickHandler = event => {
    setState(!active)
    dipatchTagEvent(tagName, event.target, !active)
  }

  return (
    <button
      key={key}
      className={`${styles.tag} ${active ? styles.active : ``}`}
      onClick={clickHandler}
    >
      <span sx={{ fontWeight: 300 }}>{tagName}</span>
      <span sx={{ fontWeight: 100 }} className={styles.tagNumber}>
        {articles}
      </span>
    </button>
  )
}

/**
 * @param {{
 *  articles: {
 *    title: string,
 *    authors: { tweag: boolean, name: string, ref?: string }
 *    date: Date,
 *    status: string,
 *    tags: Set<string>,
 *    links: Array<Array<[string, string]>>,
 *    pdf: string,
 *  }[],
 *  tags: {name: string, articles: number}[]
 * }} props
 * @returns {JSX.Element}
 */
const ArticleBox = ({ articles, tags }) => {
  const initialState = {
    articles: articles,
    tags: tags,
    activeTags: new Set(),
  }

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
   *  tags: {name: string, articles: number}[],
   *  activeTags: Set<string>
   * }} state
   * @param {{ tag: string, status: boolean }} action
   */
  const filterReducer = (state, action) => {
    let finalActiveTags

    if (action.status) {
      finalActiveTags = state.activeTags
      finalActiveTags.add(action.tag)
    } else {
      finalActiveTags = state.activeTags
      finalActiveTags.delete(action.tag)
    }

    const finalArticles = articles.filter(article => {
      let isCompatible = true

      for (const activeTag of finalActiveTags) {
        let isCompatibleWithActiveTag = false

        for (const articleTag of article.tags) {
          isCompatibleWithActiveTag =
            isCompatibleWithActiveTag || articleTag === activeTag
        }

        isCompatible = isCompatible && isCompatibleWithActiveTag
      }

      return isCompatible
    })

    const finalTags = countTags(finalArticles, tags)

    return {
      articles: finalArticles,
      tags: finalTags,
      activeTags: finalActiveTags,
    }
  }

  const [state, filter] = useReducer(filterReducer, initialState)

  const mainRef = useRef()
  useTagToggledEventMonitor(mainRef, event => {
    filter({ tag: event.tagName, status: event.state })
  })

  useFirstArticleToggler(mainRef)

  const articlesByYear = Object.entries(
    classifyArray(state.articles, article => String(article.date.getFullYear()))
  )

  articlesByYear.sort(
    ([year1], [year2]) => (Number(year1) - Number(year2)) * -1
  )

  return (
    <div className={styles.articleBox} ref={mainRef}>
      <div className={styles.tagBox}>
        {state.tags.map((tag, index) => (
          <TagButton
            key={tag.name}
            articles={tag.articles}
            tagName={tag.name}
          />
        ))}
      </div>
      <div className={styles.articles}>
        {articlesByYear.map(([year, articles]) => (
          <Year year={year} articles={articles} key={year} />
        ))}
      </div>
    </div>
  )
}

export default ArticleBox

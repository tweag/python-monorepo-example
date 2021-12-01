/* eslint-disable no-invalid-this */
import { createContext, useRef, useState, useContext } from "react"
import lunr from "lunr"

export const SearchContext = createContext(null)

/**
 * @param {Array<{
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string,
 *    shortDescription: string,
 *    links: {[linkName: string]: string},
 *  }>} data
 */
export function generateLunrIndex(data) {
  return lunr(function () {
    this.ref(`slug`)
    this.field(`name`)
    this.field(`bio`)
    this.field(`tags`)
    this.field(`shortDescription`)

    data.forEach(person => this.add(person))
  })
}

class SearchManager {
  /**
   * @param {lunr.Index} lunrIndex
   * @param {() => void} reRender
   */
  constructor(lunrIndex, reRender) {
    this.index = lunrIndex
    this.reRender = reRender
    this.searchString = ``
    this._tags = new Set()
    this.activeProfiles = this.internalSearch(this.searchString)
  }

  get tags() {
    return Array.from(this._tags)
      .map(tag => `+${tag}`)
      .join(` `)
  }

  /**
   * @param {string} tag
   * @returns {boolean}
   */
  containsTag(tag) {
    return this._tags.has(tag)
  }

  /**
   * @param {string} searchString
   */
  internalSearch(searchString) {
    const finalSearchString = [this.tags, searchString].join(` `)
    console.log(`String being used in the search: ${finalSearchString}`)
    return this.index.search(finalSearchString).map(item => item.ref)
  }

  updateActiveProfiles() {
    this.activeProfiles = this.internalSearch(this.searchString)
  }

  /**
   * @param {string} tag
   */
  addTag(tag) {
    this._tags.add(tag)
    this.updateActiveProfiles()
    this.reRender()
  }

  /**
   * @param {string} tag
   */
  removeTag(tag) {
    this._tags.delete(tag)
    this.updateActiveProfiles()
    this.reRender()
  }

  /**
   * @param {string} searchString
   */
  filter(searchString) {
    this.searchString = searchString
    this.updateActiveProfiles()
    console.log(`Active tags: ${Array.from(this._tags).join(` `)}`)
    console.log(`Matching profiles: ${this.activeProfiles.length}`)
    this.reRender()
  }

  clear() {
    this._tags.clear()
    this.filter(``)
  }
}

/**
 * @returns {SearchManager}
 */
export function useSearchManager(data) {
  const searchIndexRef = useRef(null)
  const [, reRender] = useState({})

  if (!searchIndexRef.current) {
    searchIndexRef.current = new SearchManager(generateLunrIndex(data), () =>
      reRender({})
    )
  }

  return searchIndexRef.current
}

/**
 * @returns {SearchManager}
 */
export function useSearchContext() {
  const manager = useContext(SearchContext)
  return manager
}

/**
 * @param {HTMLElement} source
 * @param {string} filterString
 */
export function dispatchFilterEvent(source, filterString) {
  const event = new Event(`filter`, { bubbles: true })
  event.filterString = filterString
  source.dispatchEvent(event)
}

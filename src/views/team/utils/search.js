/* eslint-disable no-invalid-this */
import { createContext } from "react"
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

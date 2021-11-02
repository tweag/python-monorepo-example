/**
 * @param {{
 *    title: string,
 *    status: string,
 *    date: number | string,
 *    abstract: string,
 *    tags: string[],
 *    links: {[linkName: string]: string},
 *    authors: string[]
 *  }[]} papers
 * @param {string[]} tweagers
 * @param {{ name: string, publicURL: string }[]} files
 * @returns {{
 *  articles: {
 *    title: string,
 *    authors: { tweag: boolean, name: string }
 *    date: Date,
 *    status: string,
 *    tags: Set<string>,
 *    links: Array<[string, string]>,
 *  }[],
 *  tags: {name: string, articles: number}[]
 * }}
 */
export function parsePapers(papers, tweagers, files) {
  const result = { articles: [], tags: [] }
  const tags = {}

  for (const paper of papers) {
    const toAdd = { ...paper }

    // Parse date
    toAdd.date = new Date(String(toAdd.date))
    toAdd.links = Object.entries(toAdd.links)

    // Replace links
    for (const link of toAdd.links ?? []) {
      link[1] = files[link[1]] ?? link[1]
    }

    // Parse authors
    toAdd.authors = toAdd.authors.map(author => ({
      name: author,
      tweag: tweagers.includes(author),
    }))

    // Process and count tags
    toAdd.tags = new Set(toAdd.tags)
    toAdd.tags.forEach(tag => {
      if (Object.keys(tags).includes(tag)) {
        tags[tag] += 1
      } else {
        tags[tag] = 1
      }
    })

    result.tags = Object.keys(tags).map(tag => ({
      name: tag,
      articles: tags[tag],
    }))
    result.articles.push(toAdd)
  }

  result.articles.sort(
    (article1, article2) =>
      (article1.date.getTime() - article2.date.getTime()) * -1
  )

  return result
}

/**
 * @param {{
 *    title: string,
 *    authors: { tweag: boolean, name: string, ref?: string }
 *    date: Date,
 *    status: string,
 *    tags: Set<string>,
 *    links: Array<Array<[string, string]>>,
 *  }[]} articles
 * @param {{name: string, articles: number}[]} tags
 * @returns {{name: string, articles: number}[]}
 */
export function countTags(articles, tags) {
  const newTags = {}

  tags.forEach(({ name }) => {
    newTags[name] = 0
  })

  for (const article of articles) {
    article.tags.forEach(tag => {
      newTags[tag] += 1
    })
  }

  return Object.entries(newTags).map(entry => ({
    name: entry[0],
    articles: entry[1],
  }))
}

/**
 *  This function is meant to classify items inside an array.
 * @param {Array<any>} arrayToClassify
 * @param {(item: any, index: number, array: Array<any>) => string} classifierFunction - This function is used when iterating over the array, it must return a value with a classification for the item
 * @returns {[classifications: string]: Array<any>} - An object in which the keys will be the strings returned by the classifier funtion, the values will be arrays with the items belonging to the classification.
 */
export function classifyArray(arrayToClassify, classifierFunction) {
  const result = {}

  arrayToClassify.forEach((item, index, array) => {
    const classification = classifierFunction(item, index, array)
    if (Object.keys(result).includes(classification)) {
      result[classification].push(item)
    } else {
      result[classification] = [item]
    }
  })

  return result
}

export function getFullMonth(monthNumberString) {
  // Be careful when using Intl object on node, it doesn't work properly
  const months = {
    "0": `January`,
    "1": `February`,
    "2": `March`,
    "3": `April`,
    "4": `May`,
    "5": `June`,
    "6": `July`,
    "7": `August`,
    "8": `September`,
    "9": `October`,
    "10": `November`,
    "11": `December`,
  }
  return months[monthNumberString]
}

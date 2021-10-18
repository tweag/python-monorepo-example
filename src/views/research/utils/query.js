import YAML from "yaml"

/**
 * @returns {{
 *  tweagers: string[],
 *  files: {[filename: string]: string},
 *  papers: {
 *    title: string,
 *    status: string,
 *    date: number | string,
 *    abstract: string,
 *    tags: string[],
 *    links: {[linkName: string]: string},
 *    authors: string[]
 *  }[]
 * }}
 */
export function parseData(data) {
  const index = YAML.parse(data.file.internal.content)
  const tweagers = index.tweagers
  const papers = index.papers

  const files = {}
  for (const { node } of data.allFile.edges) {
    files[`${node.name}.${node.extension}`] = node.publicURL
  }

  return { tweagers, papers, files }
}

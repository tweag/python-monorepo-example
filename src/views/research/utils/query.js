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
 *    links: Array<[string, string]>
 *    authors: string[]
 *  }[]
 * }}
 */
export function parseData(data) {
  const tweagers = data.file.childPapersYaml.tweagers
  const papers = data.file.childPapersYaml.papers

  const files = {}
  for (const { node } of data.allFile.edges) {
    files[`${node.name}.${node.extension}`] = node.publicURL
  }

  return { tweagers, papers, files }
}

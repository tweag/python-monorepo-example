const fs = require(`fs`).promises
const YAML = require(`yaml`)
const path = require(`path`)

function convertLinks(links) {
  const result = {}
  for (const [name, url] of links) {
    result[name] = url
  }

  return result
}

async function main() {
  const index = YAML.parse(
    await fs.readFile(path.resolve(`content/papers/index.yaml`), `utf-8`)
  )

  for (const paper of index.papers) {
    paper.links = convertLinks(paper.links ?? [])
  }

  await fs.writeFile(
    path.resolve(`content/papers/index.yaml`),
    YAML.stringify(index),
    {
      encoding: `utf-8`,
    }
  )
}

main()

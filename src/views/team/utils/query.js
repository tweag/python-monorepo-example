export function parsePhotos(data) {
  const result = {}

  for (const { node } of data.allFile.edges) {
    result[node.name] = node.children[0].fluid.srcWebp
  }

  return result
}

/**
 * @returns {{
 *  name: string,
 *  bio: string,
 *  role: string,
 *  tags: string[],
 *  slug: string
 * }[]}
 */
export function parseProfiles(data) {
  const profiles = []

  for (const { node } of data.allProfilesYaml.edges) {
    const profile = {
      name: node.name,
      bio: node.bio,
      role: node.experience.find(experience => experience.employer === `Tweag`),
      tags: node.skills ?? [],
      slug: node.slug,
    }

    profiles.push(profile)
  }

  return profiles
}

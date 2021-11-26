import YAML from "yaml"

export function parsePhotos(data) {
  const result = {}

  for (const { node } of data.profileImages.edges) {
    result[node.name] = node.children[0].fixed.src
  }

  return result
}

/**
 * @returns {{
 *  name: string,
 *  bio: string,
 *  role: string,
 *  tags: string[],
 *  slug: string,
 *  shortDescription: string,
 *  links: {[linkName: string]: string}
 * }[]}
 */
export function parseProfiles(data) {
  const profiles = []

  for (const { node } of data.profiles.edges) {
    const queriedProfile = YAML.parse(node.internal.content)
    const links = queriedProfile.links ?? {}
    links.CV = `/cv/${queriedProfile.slug}`
    const profile = {
      name: queriedProfile.name,
      bio: queriedProfile.bio ?? ``,
      role: queriedProfile.experience ?
            queriedProfile.experience.find(experience => experience.employer === `Tweag`)?.role :
            ``
            ,
      tags: queriedProfile.skills ?? [],
      slug: queriedProfile.slug,
      shortDescription: queriedProfile.shortDescription ?? ``,
      links: links,
    }

    profiles.push(profile)
  }

  return profiles
}

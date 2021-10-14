/** @jsx jsx */
import { jsx } from "theme-ui"
import { ProfileTile, TagTile, ColorTile } from "./tiles"

/**
 * @param {{
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string
 *  }[]} people
 * @param {{[person: string]: string}} photos
 * @param {string[]} tags
 */
export function spawnTiles(people, photos, tags) {
  const allTiles = []

  const peopleWithPhotos = people.filter(person => !!photos[person.slug])

  allTiles.push(...tags.map(tag => <TagTile tag={tag} key={`tag:${tag}`} />))

  allTiles.push(
    ...peopleWithPhotos.map(person => (
      <ProfileTile
        person={person}
        photo={photos[person.slug]}
        key={`profile:${person.slug + person.name}`}
      />
    ))
  )

  for (let i = 0; i < 60; i++) {
    allTiles.push(<ColorTile key={`color:${i}`} />)
  }

  return allTiles
}

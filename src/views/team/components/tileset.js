/** @jsx jsx */
import { jsx } from "theme-ui"
import { allocateTiles } from "../utils/randomizers"
import { gridify } from "../utils/ajustments"
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
  const result = []

  const peopleWithPhotos = people.filter(person => !!photos[person.slug])

  const smallProfiles = Math.floor(peopleWithPhotos.length * 0.7)
  const bigProfiles = peopleWithPhotos.length - smallProfiles
  const tiles = allocateTiles({
    color: 20,
    profile: smallProfiles,
    bigProfile: bigProfiles,
    tag: tags.length,
  })

  let currentPerson = 0
  let currentTag = 0
  let currentColor = 0

  for (const tile of tiles) {
    switch (tile.tileType) {
      case `color`:
        result.push(<ColorTile key={`tag:${currentColor}`} />)
        currentColor++
        break
      case `profile`:
        result.push(
          <ProfileTile
            person={peopleWithPhotos[currentPerson]}
            photo={photos[peopleWithPhotos[currentPerson].slug]}
            key={`profile:${peopleWithPhotos[currentPerson].slug}`}
          />
        )
        currentPerson++
        break
      case `tag`:
        result.push(
          <TagTile tag={tags[currentTag]} key={`tag:${tags[currentTag]}`} />
        )
        currentTag++
    }
  }

  const randomAllocation = allocateTiles(tiles)
  let personToAdd

  const toGridify = randomAllocation.map(({ tileType }, index) => {
    switch (tileType) {
      case `color`:
        return { height: 1, width: 1, type: `color`, id: `color:${index}` }
      case `profile`:
        personToAdd = peopleWithPhotos.splice(
          Math.floor(peopleWithPhotos.length * Math.random()),
          1
        )[0]
        return {
          height: 1,
          width: 1,
          type: `profile`,
          id: `profile:${personToAdd.slug}`,
        }
      case `bigProfile`:
        personToAdd = peopleWithPhotos.splice(
          Math.floor(peopleWithPhotos.length * Math.random()),
          1
        )[0]
        return {
          height: 2,
          width: 2,
          type: `bigProfile`,
          id: `bigProfile:${personToAdd.slug}`,
        }
      case `tag`:
        return {
          height: 1,
          width: 1,
          type: `tag`,
          id: `tag:${tags.pop()}`,
        }
    }
  })

  const gridifiedTiles = gridify(toGridify)

  let personToGenerateTile
  let personSlug
  currentColor = 0
  currentTag = 0

  for (const tile of gridifiedTiles) {
    if (tile.type === `bigProfile`) {
      personSlug = tile.id.match(/:(\w+)/)[1]
      personToGenerateTile = peopleWithPhotos.find(
        person => person.slug === personSlug
      )
      result.push(
        <ProfileTile
          person={personToGenerateTile}
          photo={photos[personSlug]}
          key={`profile:${personSlug}`}
          height={2}
          width={2}
          start={{
            x: tile.x,
            y: tile.y,
          }}
        />
      )
    }
  }

  for (const tile of gridifiedTiles) {
    switch (tile.type) {
      case `profile`:
        personSlug = tile.id.match(/:(\w+)/)[1]
        personToGenerateTile = peopleWithPhotos.find(
          person => person.slug === personSlug
        )
        result.push(
          <ProfileTile
            person={personToGenerateTile}
            photo={photos[personSlug]}
            key={`profile:${personSlug}`}
          />
        )
        break
      case `color`:
        result.push(<ColorTile key={`color:${currentColor}`} />)
        currentColor++
        break
      case `tag`:
        result.push(<TagTile key={`tag:${currentTag}`} />)
        currentTag++
        break
    }
  }

  return result
}

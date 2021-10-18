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

  // Step 1: Extract valid profiles
  const peopleWithPhotos = people.filter(person => !!photos[person.slug])
  const validProfiles = [...peopleWithPhotos]

  // Step 2: Define tiles proportions for each type
  const smallProfiles = Math.floor(peopleWithPhotos.length * 0.7)
  const bigProfiles = peopleWithPhotos.length - smallProfiles

  // Step 3: Randomly distribute tiles
  const tiles = allocateTiles({
    color: smallProfiles + bigProfiles + tags.length,
    profile: smallProfiles,
    bigProfile: bigProfiles,
    tag: tags.length,
  })

  // Step 4: Add information about each tile size on the random distribution
  let personToAdd
  const toGridify = tiles.map(({ tileType }, index) => {
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

  // Step 5: Run gridify to allocate each file according to its size
  const gridifiedTiles = gridify(toGridify)

  // Step 6: Add Big profiles to result
  let personToGenerateTile
  let personSlug
  let currentColor = 0
  let currentTag = ``

  for (const tile of gridifiedTiles) {
    if (tile.type === `bigProfile`) {
      personSlug = tile.id.match(/:(.+)/)[1]
      personToGenerateTile = validProfiles.find(
        person => person.slug === personSlug
      )

      if (personToGenerateTile) {
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
  }

  // Step 7: Add all other profiles to the result
  for (const tile of gridifiedTiles) {
    switch (tile.type) {
      case `profile`:
        personSlug = tile.id.match(/:(.+)/)[1]
        personToGenerateTile = validProfiles.find(
          person => person.slug === personSlug
        )

        if (personToGenerateTile) {
          result.push(
            <ProfileTile
              person={personToGenerateTile}
              photo={photos[personSlug]}
              key={`profile:${personSlug}`}
            />
          )
        }
        break
      case `color`:
        result.push(<ColorTile key={`color:${currentColor}`} />)
        currentColor++
        break
      case `tag`:
        currentTag = tile.id.match(/:(.+)/)[1]
        result.push(<TagTile tag={currentTag} key={`tag:${currentTag}`} />)
        break
    }
  }

  return result
}

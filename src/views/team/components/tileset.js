/** @jsx jsx */
import { jsx } from "theme-ui"
import { v4 as uuid } from "uuid"

import { allocateTiles } from "../utils/randomizers"
import { gridify } from "../utils/ajustments"
import { ProfileTile, TagTile, ColorTile, BlankTile } from "./tiles"
import Bio from "./bio"

/**
 * @param {{
 *    person: {
 *      name: string,
 *      bio: string,
 *      role: string,
 *      tags: string[],
 *      slug: string,
 *    },
 *    start: {
 *      x: number,
 *      y: number,
 *    },
 *    height: number,
 *    width: number,
 *    rounding: number,
 *  }} activeBioProfile
 * @param {number} columns
 * @returns {{
 *  id: string,
 *  start: {
 *    x: number,
 *    y: number
 *  },
 *  height: number,
 *  width: number,
 * }[]}
 */
function parseActiveBioProfile(activeBioProfile, columns) {
  if (!activeBioProfile) {
    return []
  }

  const tileType =
    activeBioProfile.height == 1 && activeBioProfile.width == 1
      ? `profile`
      : `bigProfile`
  const id = `${tileType}:${activeBioProfile.person.slug}`
  const resultProfile = {
    id,
    start: activeBioProfile.start,
    height: activeBioProfile.height,
    width: activeBioProfile.width,
  }

  // Calculate profile position and size
  let profilePosition = `right`
  let profileHeight = 5
  let profileWidth = 3

  const spaceToTheLeft = activeBioProfile.start.y - 1
  const spaceToTheRight =
    columns - (activeBioProfile.start.y - 1 + activeBioProfile.width)

  if (spaceToTheRight < profileWidth && spaceToTheLeft >= profileWidth) {
    profilePosition = `left`
  } else if (spaceToTheLeft < profileWidth && spaceToTheRight < profileWidth) {
    profileHeight = 8
    profileWidth = 2
    profilePosition = spaceToTheLeft < spaceToTheRight ? `right` : `left`
  }

  const profileStart =
    profilePosition === `right`
      ? {
          x: activeBioProfile.start.x,
          y: activeBioProfile.start.y + activeBioProfile.width,
        }
      : {
          x: activeBioProfile.start.x,
          y: activeBioProfile.start.y - profileWidth,
        }

  const resultBio = {
    id: `bio:${activeBioProfile.person.slug}`,
    start: profileStart,
    height: profileHeight,
    width: profileWidth,
  }

  return [resultBio, resultProfile]
}

/**
 * @param {{
 *  people: {
 *    name: string
 *    bio: string,
 *    role: string,
 *    tags: string[],
 *    slug: string
 *  }[];
 *  photos: {[person: string]: string};
 *  tags: string[];
 *  columns: number;
 *  breakpoint: string;
 *  arbitraryAllocations?: {
 *    color?: number,
 *    blank?: number,
 *  };
 *  activeBioProfile?: {
 *    person: {
 *      name: string,
 *      bio: string,
 *      role: string,
 *      tags: string[],
 *      slug: string,
 *    },
 *    start: {
 *      x: number,
 *      y: number,
 *    },
 *    height: number,
 *    width: number,
 *    rounding: number,
 *  }
 * }} options
 */
export function spawnTiles({
  people,
  photos,
  tags,
  columns,
  breakpoint,
  arbitraryAllocations,
  activeBioProfile,
}) {
  const result = []

  // Step 0: Pre-parse Active Profile
  const prePositionedStuff = parseActiveBioProfile(activeBioProfile, columns)

  // Step 0.5: Bio on mobile
  if (!!activeBioProfile && (breakpoint === `sm` || breakpoint === `xs`)) {
    return [
      <Bio
        relativePosition={
          prePositionedStuff[0].start.y < prePositionedStuff[1].start.y
            ? `left`
            : `right`
        }
        start={{ x: 1, y: 1 }}
        height={5}
        width={3}
        key={prePositionedStuff[0].id}
        rounding={activeBioProfile.rounding}
        person={activeBioProfile.person}
      />,
    ]
  }

  // Step 1: Extract valid profiles
  const peopleWithPhotos = people.filter(person => !!photos[person.slug])
  const validProfiles = [...peopleWithPhotos]

  // Step 2: Define tiles proportions for each type
  const smallProfiles = Math.floor(validProfiles.length * 0.7)
  const bigProfiles = peopleWithPhotos.length - smallProfiles

  // Step 3: Randomly distribute tiles
  const tileAllocation = {
    color: arbitraryAllocations?.color ?? bigProfiles + tags.length,
    profile: smallProfiles,
    bigProfile: bigProfiles,
    tag: tags.length,
    blank: arbitraryAllocations?.blank ?? smallProfiles,
  }
  const tiles = allocateTiles(tileAllocation)

  // Step 4: Add information about each tile size on the random distribution
  let personToAdd
  const disposableTags = [...tags]
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
          id: `tag:${disposableTags.pop()}`,
        }
      case `blank`:
        return { height: 1, width: 1, type: `blank`, id: `blank:${index}` }
    }
  })

  // Step 5: Run gridify to allocate each file according to its size
  if (activeBioProfile) {
    toGridify.splice(
      toGridify.findIndex(item =>
        new RegExp(`:${activeBioProfile.person.slug}`).test(item.id)
      ),
      1
    )
  }
  const gridifiedTiles = gridify(toGridify, prePositionedStuff, columns)

  // Step 6: Adding things with fixed position like bio and big profiles
  let personToGenerateTile
  let personSlug
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
    } else if (tile.type === `bio`) {
      const bioRelativePosition =
        prePositionedStuff[0].start.y < prePositionedStuff[1].start.y
          ? `left`
          : `right`

      result.push(
        <Bio
          relativePosition={bioRelativePosition}
          start={prePositionedStuff[0].start}
          height={prePositionedStuff[0].height}
          width={prePositionedStuff[0].width}
          key={prePositionedStuff[0].id}
          rounding={activeBioProfile.rounding}
          person={activeBioProfile.person}
        />
      )
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
              height={1}
              width={1}
              start={{
                x: tile.x,
                y: tile.y,
              }}
            />
          )
        }
        break
      case `color`:
        result.push(<ColorTile key={`color:${uuid()}`} />)
        break
      case `tag`:
        currentTag = tile.id.match(/:(.+)/)[1]
        result.push(<TagTile tag={currentTag} key={`tag:${currentTag}`} />)
        break
      case `blank`:
        result.push(<BlankTile key={`color:${uuid()}`} />)
        break
    }
  }

  return result
}

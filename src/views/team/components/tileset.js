/** @jsx jsx */
import { jsx } from "theme-ui"
import { v4 as uuid } from "uuid"

import { allocateTiles } from "../utils/randomizers"
import { gridify } from "../utils/ajustments"
import { ProfileTile, TagTile, ColorTile, BlankTile } from "./tiles"
import Bio from "./bio"

export class TileSet {
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
  constructor({
    people,
    photos,
    tags,
    columns,
    breakpoint,
    arbitraryAllocations,
    activeBioProfile,
  }) {
    this.finalTiles = []
    this.breakpoint = breakpoint
    this.tags = tags
    this.columns = columns
    this.photos = photos
    this.activeBioProfile = activeBioProfile
    this.arbitraryAllocations = arbitraryAllocations
    this.roundings = new Map()

    this.prePositionedStuff = this.parseActiveBioProfile()
    this.validProfiles = people.filter(person => !!this.photos[person.slug])
    this.tilesOrder = this.generateRandomTilesOrder()
    this.tilesSkeletons = this.generateSkeletons()
    this.tilesGrid = this.generateGrid()
    this.generateTiles()
  }

  /**
   * @param {{
   *  color: number,
   *  columns: number,
   *  blank: number,
   *  breakpoint: string,
   * }} options
   */
  updateResponsiveParameters({ color, columns, blank, breakpoint }) {
    this.arbitraryAllocations.blank = blank ?? this.arbitraryAllocations.blank
    this.arbitraryAllocations.color = color ?? this.arbitraryAllocations.color
    this.columns = columns ?? this.columns
    this.breakpoint = breakpoint ?? this.breakpoint
    this.tilesSkeletons = this.generateSkeletons()
    this.tilesGrid = this.generateGrid()
    this.generateTiles()
  }

  /**
   * @param {{
   *  person: {
   *    name: string,
   *    bio: string,
   *    role: string,
   *    tags: string[],
   *    slug: string,
   *  },
   *  start: {
   *    x: number,
   *    y: number,
   *  },
   *  height: number,
   *  width: number,
   *  rounding: number,
   * }} newProfile
   */
  setActiveProfile(newProfile) {
    this.activeBioProfile = newProfile
    this.prePositionedStuff = this.parseActiveBioProfile()
    this.tilesGrid = this.generateGrid()
    this.generateTiles()
  }

  shuffleTiles() {
    this.tilesOrder = this.generateRandomTilesOrder()
    this.tilesSkeletons = this.generateSkeletons()
    this.tilesGrid = this.generateGrid()
    this.generateTiles()
  }

  generateRoundings() {
    for (const tile of this.tilesGrid) {
      if (!this.roundings.has(tile.id)) {
        this.roundings.set(tile.id, Math.floor(Math.random() * 3))
      }
    }
  }

  /**
   * @returns {{
   *  height: number,
   *  width: number,
   *  type: string,
   *  id: string,
   * }[]}
   */
  generateSkeletons() {
    let personToAdd
    let currentColor = -1
    const peopleWithPhotos = [...this.validProfiles]
    const disposableTags = [...this.tags]
    const skeletons = this.tilesOrder.map(({ tileType }, index) => {
      switch (tileType) {
        case `color`:
          currentColor++
          return {
            height: 1,
            width: 1,
            type: `color`,
            id: `color:${currentColor}`,
          }
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

    return skeletons
  }

  generateRandomTilesOrder() {
    // Step 2: Define tiles proportions for each type
    const smallProfiles = Math.floor(this.validProfiles.length * 0.7)
    const bigProfiles = this.validProfiles.length - smallProfiles

    // Step 3: Randomly distribute tiles
    const tileAllocation = {
      color: this.arbitraryAllocations?.color ?? bigProfiles + this.tags.length,
      profile: smallProfiles,
      bigProfile: bigProfiles,
      tag: this.tags.length,
      blank: this.arbitraryAllocations?.blank ?? smallProfiles,
    }
    return allocateTiles(tileAllocation)
  }

  generateGrid() {
    let idToIgnore = ``
    if (this.activeBioProfile) {
      idToIgnore = this.tilesSkeletons.find(item =>
        new RegExp(`:${this.activeBioProfile.person.slug}`).test(item.id)
      ).id
    }

    const toGridify = this.tilesSkeletons.filter(item => item.id !== idToIgnore)
    const prePositionedItems =
      !!this.prePositionedStuff.activeBio &&
      !!this.prePositionedStuff.activeProfile
        ? [
            this.prePositionedStuff.activeBio,
            this.prePositionedStuff.activeProfile,
          ]
        : []
    const gridifiedTiles = gridify(toGridify, prePositionedItems, this.columns)
    return gridifiedTiles
  }

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
   * @returns {{
   *   activeBio: {
   *     id: string,
   *     start: {
   *       x: number,
   *       y: number
   *     },
   *     height: number,
   *     width: number,
   *   },
   *   activeProfile: {
   *     id: string,
   *     start: {
   *       x: number,
   *       y: number
   *     },
   *     height: number,
   *     width: number,
   *   },
   * }}
   */
  parseActiveBioProfile() {
    if (!this.activeBioProfile) {
      return {}
    }

    const tileType =
      this.activeBioProfile.height == 1 && this.activeBioProfile.width == 1
        ? `profile`
        : `bigProfile`
    const id = `${tileType}:${this.activeBioProfile.person.slug}`
    const resultProfile = {
      id,
      start: this.activeBioProfile.start,
      height: this.activeBioProfile.height,
      width: this.activeBioProfile.width,
    }

    // Calculate profile position and size
    let profilePosition = `right`
    let profileHeight = 5
    let profileWidth = 3

    const spaceToTheLeft = this.activeBioProfile.start.y - 1
    const spaceToTheRight =
      this.columns -
      (this.activeBioProfile.start.y - 1 + this.activeBioProfile.width)

    if (spaceToTheRight < profileWidth && spaceToTheLeft >= profileWidth) {
      profilePosition = `left`
    } else if (
      spaceToTheLeft < profileWidth &&
      spaceToTheRight < profileWidth
    ) {
      profileHeight = 8
      profileWidth = 2
      profilePosition = spaceToTheLeft < spaceToTheRight ? `right` : `left`
    }

    const profileStart =
      profilePosition === `right`
        ? {
            x: this.activeBioProfile.start.x,
            y: this.activeBioProfile.start.y + this.activeBioProfile.width,
          }
        : {
            x: this.activeBioProfile.start.x,
            y: this.activeBioProfile.start.y - profileWidth,
          }

    const resultBio = {
      id: `bio:${this.activeBioProfile.person.slug}`,
      start: profileStart,
      height: profileHeight,
      width: profileWidth,
    }

    return {
      activeProfile: resultProfile,
      activeBio: resultBio,
    }
  }

  /**
   * @param {{
   *  x: number;
   *  y: number;
   *  id: string;
   *  type: string;}} tile
   * @returns {JSX.Element}
   */
  generateBigProfile(tile) {
    const slug = tile.id.match(/:(.+)/)[1]
    const target = this.validProfiles.find(person => person.slug === slug)
    if (!this.roundings.has(tile.id)) {
      console.log(
        `Rounding not found for ${
          tile.id
        }, available roundings: ${JSON.stringify(
          Array.from(this.roundings.keys())
        )}`
      )
    }
    return (
      <ProfileTile
        person={target}
        photo={this.photos[slug]}
        key={`profile:${slug}`}
        height={2}
        width={2}
        start={{
          x: tile.x,
          y: tile.y,
        }}
        rounding={this.roundings.get(tile.id)}
      />
    )
  }

  /**
   * @param {{
   *  x: number;
   *  y: number;
   *  id: string;
   *  type: string;}} tile
   * @returns {JSX.Element}
   */
  generateSmallProfile(tile) {
    const slug = tile.id.match(/:(.+)/)[1]
    const target = this.validProfiles.find(person => person.slug === slug)
    return (
      <ProfileTile
        person={target}
        photo={this.photos[slug]}
        key={`profile:${slug}`}
        height={1}
        width={1}
        start={{
          x: tile.x,
          y: tile.y,
        }}
        rounding={this.roundings.get(tile.id)}
      />
    )
  }

  /**
   * @param {{
   *  x: number;
   *  y: number;
   *  id: string;
   *  type: string;}} tile
   * @returns {JSX.Element}
   */
  generateBio(tile) {
    const bioRelativePosition =
      this.prePositionedStuff.activeBio.start.y <
      this.prePositionedStuff.activeProfile.start.y
        ? `left`
        : `right`

    return (
      <Bio
        relativePosition={bioRelativePosition}
        start={this.prePositionedStuff.activeBio.start}
        height={this.prePositionedStuff.activeBio.height}
        width={this.prePositionedStuff.activeBio.width}
        key={this.prePositionedStuff.activeBio.id}
        rounding={this.activeBioProfile.rounding}
        person={this.activeBioProfile.person}
      />
    )
  }

  addPositionedTilesToFinalTiles() {
    for (const tile of this.tilesGrid) {
      switch (tile.type) {
        case `bigProfile`:
          this.finalTiles.push(this.generateBigProfile(tile))
          break
        case `profile`:
          this.finalTiles.push(this.generateSmallProfile(tile))
          break
        case `bio`:
          this.finalTiles.push(this.generateBio(tile))
          break
      }
    }
  }

  addUnpositionedTilesToFinalTiles() {
    let currentTag = ``
    let currentColor = ``
    for (const tile of this.tilesGrid) {
      switch (tile.type) {
        case `color`:
          currentColor = tile.id.match(/:(.+)/)[1]
          this.finalTiles.push(
            <ColorTile
              key={`color:${currentColor}`}
              rounding={this.roundings.get(tile.id)}
            />
          )
          break
        case `tag`:
          currentTag = tile.id.match(/:(.+)/)[1]
          this.finalTiles.push(
            <TagTile
              tag={currentTag}
              key={`tag:${currentTag}`}
              rounding={this.roundings.get(tile.id)}
            />
          )
          break
        case `blank`:
          this.finalTiles.push(
            <BlankTile
              key={`color:${uuid()}`}
              rounding={this.roundings.get(tile.id)}
            />
          )
          break
      }
    }
  }

  generateTiles() {
    if (
      !!this.activeBioProfile &&
      (this.breakpoint === `sm` || this.breakpoint === `xs`)
    ) {
      this.finalTiles = [
        <Bio
          relativePosition={
            this.prePositionedStuff.activeBio.start.y <
            this.prePositionedStuff.activeProfile.start.y
              ? `left`
              : `right`
          }
          start={{ x: 1, y: 1 }}
          height={5}
          width={3}
          key={this.prePositionedStuff.activeBio.id}
          rounding={this.activeBioProfile.rounding}
          person={this.activeBioProfile.person}
        />,
      ]
    } else {
      this.generateRoundings()
      this.finalTiles = []
      this.addPositionedTilesToFinalTiles()
      this.addUnpositionedTilesToFinalTiles()
    }
  }
}

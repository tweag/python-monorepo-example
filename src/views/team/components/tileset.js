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
   *    slug: string,
   *    shortDescription: string,
   *    links: {[linkName: string]: string}
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
    this.lastActiveProfile = ``
    this.breakpoint = breakpoint
    this.tags = tags
    this.columns = columns
    this.photos = photos
    this.activeBioProfile = activeBioProfile
    this.arbitraryAllocations = arbitraryAllocations
    this.roundings = new Map()
    this.profileColors = new Map()

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
   * @returns
   */
  getBioHeight() {
    switch (this.breakpoint) {
      case `xs`:
        return 18
      case `sm`:
        return 11
      case `md`:
        return 10
      case `lg`:
        return 9
      case `xl`:
        return 9
      case `xxl`:
        return 9
    }
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
   * } | null} newProfile
   */
  setActiveProfile(newProfile) {
    this.lastActiveProfile = this.activeBioProfile?.person?.slug ?? ``
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

  generateColors() {
    for (const tile of this.tilesGrid) {
      if (
        !this.profileColors.has(tile.id) &&
        (tile.type == `profile` || tile.type == `bigProfile`)
      ) {
        this.profileColors.set(tile.id, Math.floor(Math.random() * 3))
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

  calculateTileDistributions() {
    const bigTiles = Math.floor((this.validProfiles.length * 15) / 56)
    const smallTiles = this.validProfiles.length - bigTiles
    const blankTiles =
      this.breakpoint === `sm` || this.breakpoint === `xs`
        ? 0
        : Math.floor(smallTiles * 0.28)
    const colorTiles = smallTiles
    const tagTiles = this.tags.length

    return {
      color: colorTiles,
      profile: smallTiles,
      bigProfile: bigTiles,
      tag: tagTiles,
      blank: blankTiles,
    }
  }

  generateRandomTilesOrder() {
    return allocateTiles(this.calculateTileDistributions())
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

    const finalRow = this.activeBioProfile?.start?.x
      ? this.activeBioProfile?.start?.x
      : 1

    const tileType =
      this.activeBioProfile.height == 1 && this.activeBioProfile.width == 1
        ? `profile`
        : `bigProfile`
    const id = `${tileType}:${this.activeBioProfile.person.slug}`
    const resultProfile = {
      id,
      start: {
        x: finalRow,
        y: this.activeBioProfile.start.y,
      },
      height: this.activeBioProfile.height,
      width: this.activeBioProfile.width,
    }

    // Calculate profile position and size
    let profilePosition = `right`
    let profileHeight = this.getBioHeight()
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
      profileHeight += 1
      profileWidth -= 1
      profilePosition = spaceToTheLeft < spaceToTheRight ? `right` : `left`
    }

    const profileStart =
      profilePosition === `right`
        ? {
            x: finalRow,
            y: this.activeBioProfile.start.y + this.activeBioProfile.width,
          }
        : {
            x: finalRow,
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
        colorIndex={this.profileColors.get(tile.id)}
        active={
          this.lastActiveProfile === slug ||
          this.activeBioProfile?.person?.slug === slug
        }
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
        colorIndex={this.profileColors.get(tile.id)}
        active={
          this.lastActiveProfile === slug ||
          this.activeBioProfile?.person?.slug === slug
        }
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
              colorIndex={Number(currentColor) % 3}
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
        case `empty`:
          this.finalTiles.push(
            <ColorTile
              key={`empty:${uuid()}`}
              rounding={this.roundings.get(tile.id)}
              colorIndex={Math.floor(Math.random() * 4)}
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
      this.generateColors()
      this.finalTiles = []
      this.addPositionedTilesToFinalTiles()
      this.addUnpositionedTilesToFinalTiles()
    }
  }
}

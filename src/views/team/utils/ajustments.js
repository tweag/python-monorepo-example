class VirtualGrid {
  /**
   * @param {number} columns
   * @param {{
   *  id: string,
   *  start: {
   *    x: number,
   *    y: number
   *  },
   *  height: number,
   *  width: number,
   * }[]} prePositioned
   */
  constructor(columns, prePositioned = []) {
    this.internalGrid = [[]]
    this.limitColumns = columns
    prePositioned.forEach(item =>
      this.addPrePositionedItem({
        x: item.start.x,
        y: item.start.y,
        width: item.width,
        height: item.height,
        id: item.id,
      })
    )
  }

  /**
   * @param {{
   *  width: number,
   *  height: number,
   *  id: string,
   * }} sizeAndPlace
   */
  addItem({ width, height, id }) {
    /* To-Do */
    let whereToPutIt = this.getNextVacantPosition()
    let canIPutItThere = this.verifyPlacement({
      x: whereToPutIt.x,
      y: whereToPutIt.y,
      width,
      height,
    })

    while (!canIPutItThere) {
      whereToPutIt = this.getNextVacantPosition(whereToPutIt.x, whereToPutIt.y)
      canIPutItThere = this.verifyPlacement({
        x: whereToPutIt.x,
        y: whereToPutIt.y,
        width,
        height,
      })
    }

    for (let line = whereToPutIt.x; line < whereToPutIt.x + height; line++) {
      for (
        let column = whereToPutIt.y;
        column < whereToPutIt.y + width;
        column++
      ) {
        this.internalGrid[line][column] = id
      }
    }
  }

  addPrePositionedItem({ x, y, height, width, id }) {
    for (let line = x - 1; line < x - 1 + height; line++) {
      if (!this.internalGrid[line]) {
        this.internalGrid[line] = []
      }
      for (let column = y - 1; column < y - 1 + width; column++)
        this.internalGrid[line][column] = id
    }
  }

  getNextVacantPosition(x = 0, y = -1) {
    for (let column = y + 1; column < this.limitColumns; column++) {
      if (!this.internalGrid[x][column]) {
        return { x, y: column }
      }
    }

    if (!this.internalGrid[x + 1]) {
      return { x: x + 1, y: 0 }
    } else {
      for (let line = x + 1; line < this.internalGrid.length; line++) {
        if (!this.internalGrid[line]) {
          this.internalGrid[line] = []
        }
        for (let column = 0; column < this.limitColumns; column++) {
          if (!this.internalGrid[line][column]) {
            return { x: line, y: column }
          }
        }
      }
    }

    this.internalGrid[this.internalGrid.length] = []
    return { x: this.internalGrid.length - 1, y: 0 }
  }

  /**
   * @param { x: number, y: number, width: number, height: number } mesurements
   * @returns { boolean }
   */
  verifyPlacement({ x, y, width, height }) {
    for (let line = x; line < x + height; line++) {
      if (!this.internalGrid[line]) {
        this.internalGrid[line] = []
      }
      for (let column = y; column < y + width; column++) {
        if (this.internalGrid[line][column] || column >= this.limitColumns) {
          return false
        }
      }
    }

    return true
  }

  /**
   * @param {number} x
   * @param {number} y
   * @param {string} id
   */
  setPosition(x, y, id) {
    if (!this.internalGrid[y]) {
      this.internalGrid[y] = []
    }

    this.internalGrid[y][x] = id
  }

  dumpOrder() {
    const counted = []
    const result = []

    for (let line = 0; line < this.internalGrid.length; line++) {
      if (!this.internalGrid[line]) {
        this.internalGrid[line] = []
      }
      for (let column = 0; column < this.limitColumns; column++) {
        if (
          !!this.internalGrid[line][column] &&
          !counted.includes(this.internalGrid[line][column])
        ) {
          result.push({
            type: this.internalGrid[line][column].match(/(\w+):/)[1],
            id: this.internalGrid[line][column],
            x: line + 1,
            y: column + 1,
          })
          counted.push(this.internalGrid[line][column])
        }
      }
    }

    return result
  }
}

/**
 * @param {{
 *  height: number,
 *  width: number,
 *  id: string,
 *  type: string,
 * }[]} tiles
 * @param {{
 *  id: string,
 *  start: {
 *    x: number,
 *    y: number
 *  },
 *  height: number,
 *  width: number,
 * }[]} prePositioned
 * @param {number} columns
 * @returns {{
 *  x: number,
 *  y: number,
 *  id: string,
 *  type: string,
 * }[]}
 */
export function gridify(tiles, prePositioned = [], columns = 6) {
  const virtualGrid = new VirtualGrid(columns, prePositioned)
  for (const tile of tiles) {
    virtualGrid.addItem(tile)
  }
  return virtualGrid.dumpOrder()
}

/**
 * @param {{x: number, y: number}} start
 * @param {number} height
 * @param {number} width
 * @returns {{
 *  "--start-column": number,
 *  "--end-column": number,
 *  "--start-row": number,
 *  "--end-row": number
 * } | {}}
 */
export function parsePositionalStyles(start, width, height) {
  const result = {}

  if (!!start && !!height && !!width) {
    result[`--start-column`] = start.y
    result[`--end-column`] = start.y + width
    result[`--start-row`] = start.x
    result[`--end-row`] = start.x + height
  }

  return result
}
class VirtualGrid {
  /**
   * @param {number} columns
   */
  constructor(columns) {
    this.internalGrid = [[]]
    this.limitColumns = columns
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

  getNextVacantPosition(x = 0, y = -1) {
    for (let column = y + 1; column < this.internalGrid[x].length; column++) {
      if (!this.internalGrid[x][column]) {
        return { x, y: column }
      }
    }

    if (!this.internalGrid[x + 1]) {
      return { x: x + 1, y: 0 }
    } else {
      for (let line = x + 1; line < this.internalGrid.length; line++) {
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
      for (
        let column = y;
        column < column + width && column < this.limitColumns;
        column++
      ) {
        if (this.internalGrid[line][column]) {
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
    let lastItem
    const result = []

    for (let line = 0; line < this.internalGrid.length; line++) {
      for (let column = 0; column < this.internalGrid[line].length; column++) {
        if (!(this.internalGrid[line][column] === lastItem)) {
          result.push({
            type: this.internalGrid[line][column].match(/(\w+):/)[1],
            id: this.internalGrid[line][column],
            x: line,
            y: column,
          })
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
 * @returns {{
 *  x: number,
 *  y: number,
 *  id: string,
 *  type: string,
 * }[]}
 */
export function gridify(tiles, columns) {
  const virtualGrid = new VirtualGrid(columns)
  for (const tile of tiles) {
    virtualGrid.addItem(tile)
  }
  return virtualGrid.dumpOrder()
}

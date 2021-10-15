class Lottery {
  /**
   * @param {{[participant: string]: number}} initialOdds
   */
  constructor(initialOdds) {
    this.remainingDraws = initialOdds
    this.currentOdds = initialOdds
  }

  /**
   * @returns {number}
   */
  get totalRemainingDraws() {
    let result = 0

    for (const draws of Object.values(this.remainingDraws)) {
      result += draws
    }

    return result
  }

  draw() {
    const winningNumber = Math.floor(Math.random() * this.totalRemainingDraws)
    const participants = Object.keys(this.remainingDraws).filter(
      participant => this.remainingDraws[participant] > 0
    )
    let winner = ``
    let sum = 0

    for (const participant of participants) {
      sum += this.currentOdds[participant]
      if (sum >= winningNumber) {
        winner = participant

        // ajust chances
        this.remainingDraws[winner]--
        this.currentOdds = { ...this.remainingDraws }
        this.currentOdds[winner] =
          this.currentOdds[winner] / 2 < 1 ? 1 : this.currentOdds[winner] / 2
        break
      }
    }

    if (winner === ``) {
      winner = participants[participants.length - 1]
      this.remainingDraws[winner]--
      this.currentOdds = { ...this.remainingDraws }
      this.currentOdds[winner] =
        this.currentOdds[winner] / 2 < 0.0001
          ? 0.0001
          : this.currentOdds[winner] / 2
    }

    return winner
  }
}

/**
 * @param {{[tileType: string]: number}} tiles
 * @param {number} columns
 * @returns {{ tileType: string }[]}
 */
export function allocateTiles(tiles) {
  const result = []
  const lottery = new Lottery(tiles)
  const runs = Object.values(tiles).reduce(
    (previous, current) => previous + current
  )

  for (let i = 0; i < runs; i++) {
    result.push({ tileType: lottery.draw() })
  }

  return result
}

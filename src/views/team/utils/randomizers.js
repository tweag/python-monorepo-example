export function shuffleArray(arrayToShuffle) {
  for (let i = arrayToShuffle.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1))

    arrayToShuffle[i] = arrayToShuffle[j]
    arrayToShuffle[j] = arrayToShuffle[i]
  }
  return arrayToShuffle
}

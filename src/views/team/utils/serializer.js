/**
 * @param {Blob} input
 * @returns {Promise<string>}
 */
export function blobToDataUrl(input) {
  const reader = new FileReader()
  reader.readAsDataURL(input)
  return new Promise(resolve => {
    reader.addEventListener(`load`, () => resolve(reader.result))
  })
}

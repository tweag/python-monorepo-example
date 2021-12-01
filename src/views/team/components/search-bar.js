import React, { useState, useEffect } from "react"
import styles from "../styles/search-bar.module.css"

const filterEventIssuer = event => {
  const eventSource = event.target
  const eventToFire = new Event(`filter`, { bubbles: true })
  eventToFire.filterString = eventSource.value
  eventSource.dispatchEvent(eventToFire)
}

const SearchBar = ({ placeholder }) => {
  const [value, setValue] = useState(``)

  useEffect(() => {
    const onFilterCallback = event => {
      const newValue = event.filterString
      setValue(newValue)
    }
    window.addEventListener(`filter`, onFilterCallback)
    return () => window.removeEventListener(`filter`, onFilterCallback)
  }, [])
  return (
    <input
      name="search"
      id="searchFilter"
      type="text"
      value={value}
      className={styles.searchBar}
      placeholder={placeholder}
      onInput={filterEventIssuer}
    />
  )
}

export default SearchBar

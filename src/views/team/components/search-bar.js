import React from "react"
import styles from "../styles/search-bar.module.css"

const filterEventIssuer = event => {
  const eventSource = event.target
  const eventToFire = new Event(`filter`, { bubbles: true })
  eventToFire.filterString = eventSource.value
  eventSource.dispatchEvent(eventToFire)
}

const SearchBar = ({ placeholder }) => {
  return (
    <input
      name="search"
      id="searchFilter"
      type="text"
      className={styles.searchBar}
      placeholder={placeholder}
      onInput={filterEventIssuer}
    />
  )
}

export default SearchBar

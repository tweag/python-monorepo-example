/** @jsx jsx */
import { jsx, useThemeUI } from "theme-ui"
import { useState, useEffect } from "react"

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

  const { theme: t } = useThemeUI()
  return (
    <input
      name="search"
      id="searchFilter"
      type="text"
      value={value}
      className="searchBar"
      placeholder={placeholder}
      onInput={filterEventIssuer}
      css={`
        border: none;
        border-bottom: solid darkgray 0.1rem;
        outline: none;
        width: 15rem;
        background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='darkgray' class='bi bi-search' viewBox='0 0 16 16'%3E%3Cpath d='M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001c.03.04.062.078.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1.007 1.007 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z'/%3E%3C/svg%3E");
        background-repeat: no-repeat;
        background-origin: padding-box;
        background-position: left center;
        background-size: 1.2rem;
        padding-left: 2rem;
        transition: all 0.5s;

        &:focus {
          outline: none;
          background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='black' class='bi bi-search' viewBox='0 0 16 16'%3E%3Cpath d='M11.742 10.344a6.5 6.5 0 1 0-1.397 1.398h-.001c.03.04.062.078.098.115l3.85 3.85a1 1 0 0 0 1.415-1.414l-3.85-3.85a1.007 1.007 0 0 0-.115-.1zM12 6.5a5.5 5.5 0 1 1-11 0 5.5 5.5 0 0 1 11 0z'/%3E%3C/svg%3E");
          border-bottom: solid black 0.1rem;
        }

        @media screen and (max-width: ${t.breakpoints[1]}) {
          width: 100%;
          height: 2rem;
          margin: 0.3rem;
        }
      `}
    />
  )
}

export default SearchBar

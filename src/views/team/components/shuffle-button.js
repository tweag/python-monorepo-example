/** @jsx jsx */
import { jsx, useThemeUI } from "theme-ui"
import { Global } from "@emotion/react"

/**
 * @param {MouseEvent} event
 */
function shuffleButtonAnimationHandler(event) {
  const button = event.target
  button.classList.add(`rotateShuffleButton`)
  button.addEventListener(
    `animationend`,
    () => button.classList.remove(`rotateShuffleButton`),
    { once: true }
  )
}

const ShuffleButton = ({ onClick, ref, className }) => {
  const clickEventHandler = event => {
    if (onClick) {
      onClick(event)
    }
    shuffleButtonAnimationHandler(event)
  }
  const { theme: t } = useThemeUI()
  return (
    <>
      <Global
        styles={`
        .rotateShuffleButton::before {
          animation: rotate 1s;
        }

        .hide {
          visibility: hidden;
        }

        @keyframes rotate {
          from {
            transform: rotate(0deg);
          }
          to {
            transform: rotate(360deg);
          }
        }
        `}
      />
      <a
        className={[`shuffleButton`, className].join(` `)}
        ref={ref}
        onClick={clickEventHandler}
        css={`
          display: flex;
          flex-direction: row;
          border: solid black 0.15rem;
          border-radius: 50rem;
          padding: 0.7rem 1rem;
          cursor: pointer;

          &::before {
            display: block;
            content: "";
            width: 1.2rem;
            background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='black' class='bi bi-arrow-repeat' viewBox='0 0 16 16'%3E%3Cpath d='M11.534 7h3.932a.25.25 0 0 1 .192.41l-1.966 2.36a.25.25 0 0 1-.384 0l-1.966-2.36a.25.25 0 0 1 .192-.41zm-11 2h3.932a.25.25 0 0 0 .192-.41L2.692 6.23a.25.25 0 0 0-.384 0L.342 8.59A.25.25 0 0 0 .534 9z'/%3E%3Cpath fill-rule='evenodd' d='M8 3c-1.552 0-2.94.707-3.857 1.818a.5.5 0 1 1-.771-.636A6.002 6.002 0 0 1 13.917 7H12.9A5.002 5.002 0 0 0 8 3zM3.1 9a5.002 5.002 0 0 0 8.757 2.182.5.5 0 1 1 .771.636A6.002 6.002 0 0 1 2.083 9H3.1z'/%3E%3C/svg%3E");
            background-repeat: no-repeat;
            background-size: 1.2rem;
            margin-right: 0.5rem;
          }

          &:active {
            color: white;
            background-color: black;
          }

          &:active::before {
            background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='white' class='bi bi-arrow-repeat' viewBox='0 0 16 16'%3E%3Cpath d='M11.534 7h3.932a.25.25 0 0 1 .192.41l-1.966 2.36a.25.25 0 0 1-.384 0l-1.966-2.36a.25.25 0 0 1 .192-.41zm-11 2h3.932a.25.25 0 0 0 .192-.41L2.692 6.23a.25.25 0 0 0-.384 0L.342 8.59A.25.25 0 0 0 .534 9z'/%3E%3Cpath fill-rule='evenodd' d='M8 3c-1.552 0-2.94.707-3.857 1.818a.5.5 0 1 1-.771-.636A6.002 6.002 0 0 1 13.917 7H12.9A5.002 5.002 0 0 0 8 3zM3.1 9a5.002 5.002 0 0 0 8.757 2.182.5.5 0 1 1 .771.636A6.002 6.002 0 0 1 2.083 9H3.1z'/%3E%3C/svg%3E");
          }

          @media screen and (hover) {
            &:hover {
              color: white;
              background-color: black;
            }

            &:hover::before {
              background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='16' height='16' fill='white' class='bi bi-arrow-repeat' viewBox='0 0 16 16'%3E%3Cpath d='M11.534 7h3.932a.25.25 0 0 1 .192.41l-1.966 2.36a.25.25 0 0 1-.384 0l-1.966-2.36a.25.25 0 0 1 .192-.41zm-11 2h3.932a.25.25 0 0 0 .192-.41L2.692 6.23a.25.25 0 0 0-.384 0L.342 8.59A.25.25 0 0 0 .534 9z'/%3E%3Cpath fill-rule='evenodd' d='M8 3c-1.552 0-2.94.707-3.857 1.818a.5.5 0 1 1-.771-.636A6.002 6.002 0 0 1 13.917 7H12.9A5.002 5.002 0 0 0 8 3zM3.1 9a5.002 5.002 0 0 0 8.757 2.182.5.5 0 1 1 .771.636A6.002 6.002 0 0 1 2.083 9H3.1z'/%3E%3C/svg%3E");
            }
          }

          @media screen and (max-width: ${t.breakpoints[1]}) {
            margin: 1rem 0.3rem;
            align-self: flex-end;
          }
        `}
      >
        Roll again
      </a>
    </>
  )
}

export default ShuffleButton

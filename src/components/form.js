export function submitForm(e) {
  console.log(`submitting now`)
  e.preventDefault()

  // Create the new request
  const portalid = `6929938`
  const formid = `d7302341-7237-4d17-a810-1f1148846f5e`
  const url = `https://api.hsforms.com/submissions/v3/integration/submit/${portalid}/${formid}`
  const form = document.querySelector(`#contactform`)
  const formData = new FormData(form)
  const unwantedFields = [`bot-field`, `form-name`] // these fields are added for netlify
  const entries = Array.from(formData.entries())
  const data = {
    submittedAt: Date.now().toString(),
    fields: entries
      .filter(pair => {
        return !unwantedFields.includes(pair[0])
      })
      .map(pair => {
        return { name: pair[0], value: pair[1] }
      }),
  }

  const xhr = new XMLHttpRequest()
  xhr.open(`POST`, url)
  // Sets the value of the 'Content-Type' HTTP request headers to 'application/json'
  xhr.setRequestHeader(`Content-Type`, `application/json`)

  const formmessage = document.querySelector(`#formmessage`)
  xhr.onreadystatechange = function () {
    if (xhr.readyState == 4 && xhr.status == 200) {
      formmessage.innerHTML = `Thank you for reaching out, we will soon reply to your request.`
    } else if (xhr.readyState == 4 && xhr.status == 400) {
      formmessage.innerHTML = `Please fill in a valid email address and a message.`
      console.log(xhr.responseText) // Returns a 400 error if the submission is rejected.
    } else if (xhr.readyState == 4 && xhr.status == 403) {
      formmessage.innerHTML = `No connection to our systems, please try later.`
      console.log(xhr.responseText) // Returns a 403 error if the portal isn't allowed to post submissions.
    } else if (xhr.readyState == 4 && xhr.status == 404) {
      formmessage.innerHTML = `No connection to our systems, please try later.`
      console.log(xhr.responseText) // Returns a 404 error if the formGuid isn't found
    }
    formmessage.style.visibility = `visible`
  }

  xhr.send(JSON.stringify(data))
}

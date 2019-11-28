import htm
  from "./vendor/htm.js"

// This is a "Hyperscript" callback for the htm module.
// It just creates DOM elements directly, no "VDOM."
export let h = (type, attrs, ...children) => {
  let element = document.createElement(type)

  for (let [k, v] of Object.entries(attrs || {})) {
    if (typeof v == "function")
      // E.g. "onclick".
      element[k] = v
    else
      // E.g. "checked".
      element.setAttribute(k, v)
  }

  for (let x of children) {
    try {
      if (typeof x == "string")
        element.appendChild(document.createTextNode(x))
      else if (Array.isArray(x)) {
        x.forEach(y => element.appendChild(
          typeof y === "string" ? document.createTextNode(y) : y
        ))
      }
      else
        element.appendChild(x)
    } catch (e) {
      console.error(type, x, e)
      throw e
    }
  }

  return element
}

// See https://github.com/developit/htm for usage.
export let html = htm.bind(h)

import {
  packages,
  isSymbol,
  makePackage,
  ctx,
} from "./01-lisp.js"

import { keval, execute }
  from "./02-keval.js"
import { readFromString }
  from "./03-read.js"
import { h, html }
  from "./11-html.js"

import { draw } from "./300-term.js"

window.packages = packages

ctx.print = x => console.log(x)

async function load(url) {
  let result = await fetch(url)
  let code = await result.text()
  let term = readFromString(ctx, code)

  execute({ ctx, term, limit: 1000 })
}

async function loadAll() {
  for (let e of document.querySelectorAll("script[type=lisp]")) {
    await load(e.getAttribute("HREF"))
  }
}

loadAll()

let observer = new MutationObserver(
  function (mutations) {
    for (let m of mutations) {
      for (let e of m.addedNodes) {
        if (e.tagName === "SCRIPT" &&
            e.getAttribute("TYPE") === "lisp") {
          console.log("yay", e)
          load(e.getAttribute("HREF"))
        }
      }
    }
  }
)

observer.observe(document, {
  childList: true, subtree: true })

function foo() {
  let test2 = readFromString(ctx, `
    (do
      (defgeneric foo (x))
      (defmethod foo ((x number))
        (do
          (print "number")
          (print 1)))
      (foo 1))
  `)

  let s = {
    ctx,
    term: test2,
    plan: { type: "done" },
    scope: new Map,
    scopes: [],
  }

  document.body.appendChild(h("lisp-cursor", {}, " "))
  document.body.appendChild(draw(ctx, test2))

  let cursor = document.querySelector("lisp-cursor")

  document.body.onkeypress = e => {
    if (e.key === "f" && e.ctrlKey) {
      if (document.body.contains(cursor)) {
        cursor.parentElement.insertBefore(cursor, cursor.nextSibling.nextSibling)
      } else {
        let selected = document.querySelector(".selected")
        selected.classList.remove("selected")
        selected.parentElement.insertBefore(cursor, selected.nextSibling)
      }
    }

    else if (e.key === "b" && e.ctrlKey) {
      if (document.body.contains(cursor)) {
        if (cursor.previousSibling)
          cursor.parentElement.insertBefore(cursor, cursor.previousSibling)
      } else {
        let selected = document.querySelector(".selected")
        selected.classList.remove("selected")
        selected.parentElement.insertBefore(cursor, selected)
      }
    }

    else if (e.key === "n" && e.ctrlKey) {
      if (document.body.contains(cursor)) {
        if (cursor.nextSibling) {
          cursor.nextSibling.classList.add("selected")
          cursor.remove()
        } else {
          cursor.parentElement.classList.add("selected")
          cursor.remove()
        }
      } else {
        let x = document.querySelector(".selected")
        x.classList.remove("selected")
        if (x.tagName == "LISP-LIST") {
          if (x.children.length > 0) {
            x.firstChild.classList.add("selected")
          } else {
            x.parentElement.classList.add("selected")
          }
        } else {
          if (x.nextSibling)
            x.nextSibling.classList.add("selected")
          else {
            while (x && !x.nextSibling)
              x = x.parentElement
            if (x && x.nextSibling)
              x.nextSibling.classList.add("selected")
          }
        }
      }
    }

    else if (e.key === "p" && e.ctrlKey) {
      if (document.body.contains(cursor)) {
        if (cursor.previousSibling) {
          cursor.previousSibling.classList.add("selected")
          cursor.remove()
        } else {
          let next = cursor.parentElement
          for (;;) {
            if (next.previousSibling) {
              next = next.previousSibling
              break
            } else if (next.parentElement) {
              next = next.parentElement
            } else {
              next = null
              break
            }
          }

          if (next) {
            cursor.remove()
            next.classList.add("selected")
          }
        }
      } else {
        let x = document.querySelector(".selected")
        if (x.previousSibling) {
          x.classList.remove("selected")
          x.previousSibling.classList.add("selected")
        } else {
          x.classList.remove("selected")
          x.parentElement.classList.add("selected")
        }
      }
    }
  }
}

foo()

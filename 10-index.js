import {
  packages,
  isSymbol,
  makePackage,
  ctx,
} from "./01-lisp.js"

import { keval }
  from "./02-keval.js"
import { readFromString }
  from "./03-read.js"
import { html }
  from "./11-html.js"

import { draw } from "./300-term.js"

window.packages = packages

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

  document.body.appendChild(draw(ctx, test2))
}

foo()

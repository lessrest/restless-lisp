"use strict";

let symbol = Symbol("symbol")

let PACKAGE = name => new Proxy({}, {
  get: function (o, k) {
    if (typeof k == "string") {
      if (k in o) return o[k]
      return o[k] = { [symbol]: `${name}:${k.toString()}` }
    } else
      return undefined
  }  
})

let LISP = PACKAGE("LISP")
let L = LISP

let USER = PACKAGE("USER")

let SHOW = term => {
  if (typeof term == "number")
    return term
  else if (typeof term == "string")
    return term
  else if (term[symbol])
    return term[symbol].replace(/^Symbol\((.*)\)$/, "$1")
  else if (Array.isArray(term))
    return term.map(x => SHOW(x))
}

let show = term => JSON.stringify(SHOW(term))

let EVAL = (term, scope = {}, stack = []) => {
  function bad(x) { throw new Error(`${show(L[x])} (${show(term)})`) }
  function syntax() { bad("SYNTAX-ERROR")}

  function E(term, subscope = scope, substack = stack) {
    return EVAL(term, subscope, substack)
  }

  if (typeof term == "number")
    return term
  else if (typeof term == "string")
    return term
  else if (term[symbol]) {
    let x = scope[term]
    if (x) return x
    else for (let s in stack) if (s[term]) return x
    bad("UNBOUND-VARIABLE")
  } else if (Array.isArray(term)) {
    if (term.length > 0) {
      switch (term[0]) {
      case L.IF:
        if (term.length != 3) syntax()
        return E(E(term[0]) ? term[1] : term[2])

      case L["+"]:
        if (term.length <= 1) syntax()
        let sum = 0
        for (let i = 1; i < term.length; i++)
          sum += E(term[i])
        return sum

      case L.DEFUN:
        if (term.length != 4) syntax()
        if (!term[1][symbol]) syntax()
        if (!Array.isArray(term[2])) syntax()
        if (term[2].some(x => !x[symbol])) syntax()
        return term[1][L.FUNCTION] = [term[2], term[3]]

      case L.PROGN:
        let x
        for (let subterm of term.slice(1))
          x = E(subterm)
        return x
      }
    }

    if (term[0][symbol]) {
      let λ = term[0][L.FUNCTION]
      if (λ) {
        let params = λ[0]
        let argterms = term.slice(1)
        if (argterms.length == params.length) {
          let body = λ[1]
          let args = []
          let subscope = {}
          for (let i = 0; i < params.length; i++) {
            let arg = E(argterms[i])
            subscope[params[i]] = arg
          }
          let x = EVAL(body, subscope, [scope, ...stack])
          return x
        } else {
          bad("ARGS-PARAMS-MISMATCH")
        }
      } else {
        bad("NO-DEFUN")
      }
    }

    syntax()
  }
}

console.log(
  EVAL(
    [L.PROGN,
     [L.DEFUN, USER.FOO, [USER.X, USER.Y],
      [L["+"], USER.X, USER.Y, 1]],
     [USER.FOO, 2, 2]])
)


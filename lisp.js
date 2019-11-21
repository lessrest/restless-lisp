"use strict";

export let SYMBOL = Symbol("SYMBOL")
export let SPECIAL = Symbol("SPECIAL")
export let PACKAGE_NAME = Symbol("PACKAGE-NAME")

export let PACKAGE = name => {
  let it = new Proxy({}, {
    get: function (o, k) {
      if (typeof k == "string") {
        if (k in o) return o[k]
        return o[k] = { 
          [SYMBOL]: {
            PACKAGE: it,
            NAME: k.toString().replace(/^Symbol\((.*)\)$/, "$1"),
          }
        }
      } else if (k == PACKAGE_NAME) {
        return name
      } else {
        return undefined
      }
    }  
  })
  return it
}

export let LISP = PACKAGE("LISP")
let L = LISP

function special(symbol, data = {}) { 
 symbol[SPECIAL] = data
}

special(L.IF)
special(L.LET)
special(L.DEFUN)
special(L.PROGN)
special(L.FUNCTION)
special(L["SET!"])
special(L.PRINT)

let SHOW = term => {
  if (typeof term == "number")
    return term
  if (typeof term == "boolean")
    return term
  else if (typeof term == "string")
    return term
  else if (term[SYMBOL])
    return `${term[SYMBOL].PACKAGE[PACKAGE_NAME]}:${term[SYMBOL].NAME}`
  else if (Array.isArray(term))
    return term.map(x => SHOW(x))
}

let show = term => JSON.stringify(SHOW(term))

export let EVAL = (ctx, term, scope = new Map, stack = []) => {
//  console.log("stack", stack)
  function bad(x) {
    debugger
    throw new Error(`${show(L[x])} (${show(term)})`) 
  }
  function syntax(x = term) {
    debugger
    throw new Error(`SYNTAX-ERROR (${show(x)})`)
  }
  
  function E(term, subscope = scope, substack = stack) {
    return EVAL(ctx, term, subscope, substack)
  }

  if (typeof term == "number")
    return term
  else if (typeof term == "string")
    return term
  else if (term[SYMBOL]) {
    let x = scope.get(term)
    if (x) return x
    else for (let s of stack) {
      if ((x = s.get(term)) !== undefined) return x
    }
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
        if (!term[1][SYMBOL]) syntax()
        if (!Array.isArray(term[2])) syntax()
        if (term[2].some(x => !x[SYMBOL])) syntax()
        return (
          term[1][L.FUNCTION] = {
            params: term[2],
            body: term[3],
            stack: [scope, ...stack],
          }
        )

      case L.LET: 
        {
          if (term.length != 3) syntax()
          if (!Array.isArray(term[1])) syntax()
          let subscope = new Map
          for (let x of term[1]) {
            if (!Array.isArray(x) || x.length != 2) syntax()
            if (!x[0][SYMBOL]) syntax()
            let v = E(x[1])
            subscope.set(x[0], v)
          }
          let x = EVAL(ctx, term[2], subscope, [scope, ...stack])
          return x
        }

      case L.PRINT:
        {
          if (term.length != 2) syntax()
          let x = E(term[1])
          ctx.print(x)
          return L.NIL
        }
        
      case L.PROGN: 
        {
          let x
          for (let subterm of term.slice(1))
            x = E(subterm)
          return x
        }
        
      case L["SET!"]:
        {
          if (term.length != 3) syntax()
          if (!term[1][SYMBOL]) syntax()
          let where
          if (scope.has(term[1]))
            where = scope
          else {
            for (let x of stack) {
              if (x.has(term[1])) {
                where = x
                break
              }
            }
            if (!where)
              bad("NO-SUCH-VARIABLE")
          }
          let x = E(term[2])
          where.set(term[1], x)
          return x
        }
        
      }
      
      if (term[0][SYMBOL]) {
        let λ = term[0][L.FUNCTION]
        if (λ) {
          let argterms = term.slice(1)
          if (argterms.length == λ.params.length) {
            let args = []
            let subscope = new Map
            for (let i = 0; i < λ.params.length; i++) {
              let arg = E(argterms[i])
              subscope.set(params[i], arg)
            }
            let x = EVAL(ctx, λ.body, subscope, λ.stack)
            return x
          } else {
            bad("ARGS-PARAMS-MISMATCH")
          }
        } else {
          bad("NO-DEFUN")
        }
      }
    }
  }
  syntax()
}
  
export let U = PACKAGE("DEMO")

export let example = (
  [L.PROGN,
   [L.LET, [[U.I, 0]],
    [L.DEFUN, U.TICK, [],
     [L.PROGN,
      [L["SET!"], U.I, [L["+"], U.I, 1]],
      U.I]]],
   [L.PRINT, "Ticking..."],
   [L.PRINT, [U.TICK]],
   [L.PRINT, [U.TICK]],
   [L.PRINT, [U.TICK]]]
)


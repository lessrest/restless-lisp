"use strict";

export let SYMBOL = Symbol("SYMBOL")
export let FUNCTION = Symbol("FUNCTION")
export let SPECIAL = Symbol("SPECIAL")
export let PACKAGE_NAME = Symbol("PACKAGE-NAME")
export let PACKAGE_SYMBOLS = Symbol("PACKAGE-SYMBOLS")

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
      } else if (k == PACKAGE_SYMBOLS) {
        return Object.keys(o)
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
special(L.LAMBDA)
special(L.APPLY)
special(L["SET!"])
special(L.PRINT)
special(L.NIL)
special(L["+"])
special(L["-"])

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

export let EVAL = (ctx, term, scope = new Map, stack = [], depth = 0) => {
  // console.log(" ".repeat(depth), show(term))
  
  function bad(x) {
    debugger
    throw new Error(`${show(L[x])} (${show(term)})`) 
  }
  function syntax(x = term) {
    debugger
    throw new Error(`SYNTAX-ERROR (${show(x)})`)
  }
  
  function E(term, subscope = scope, substack = stack) {
    return EVAL(ctx, term, subscope, substack, depth + 1)
  }

  if (typeof term == "number")
    return term
  else if (typeof term == "string")
    return term
  else if (term === L.NIL)
    return term
  else if (term[SYMBOL]) {
    let x = scope.get(term)
    if (x !== undefined) {
      return x
    } else {
      for (let s of stack) {
        if ((x = s.get(term)) !== undefined) return x
      }
    }
    bad("UNBOUND-VARIABLE")
  } else if (Array.isArray(term)) {
    if (term.length > 0) {
      switch (term[0]) {
      case L.IF: {
        if (term.length != 4) syntax()
        return E(E(term[1]) ? term[2] : term[3])
      }
        
      case L["+"]: {
        if (term.length <= 1) syntax()
        let x = 0
        for (let i = 1; i < term.length; i++)
          x += E(term[i])
        return x
      }
        
      case L["-"]: {
        if (term.length <= 2) syntax()
        let x = E(term[1])
        for (let i = 2; i < term.length; i++)
          x -= E(term[i])
        return x
      }
        
      case L.DEFUN: {
        if (term.length != 4) syntax()
        if (!term[1][SYMBOL]) syntax()
        if (!Array.isArray(term[2])) syntax()
        if (term[2].some(x => !x[SYMBOL])) syntax()
        return (
          term[1][FUNCTION] = {
            params: term[2],
            body: term[3],
            stack: [scope, ...stack],
          }
        )
      }

      case L.LAMBDA: {
        if (term.length != 3) syntax()
        if (!Array.isArray(term[1])) syntax()
        if (term[1].some(x => !x[SYMBOL])) syntax()
        return {
          params: term[1],
          body: term[2],
          stack: [scope, ...stack],
        }
      }

      case L.FUNCTION: {
        if (term.length != 2) syntax()
        if (!term[1][SYMBOL]) syntax()
        if (!term[1][FUNCTION]) bad("NO-FUNCTION")
        return term[1][FUNCTION]
      }

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
          let x = EVAL(ctx, term[2], subscope, [scope, ...stack], depth + 1)
          return x
        }

      case L.APPLY: {
        if (term.length != 3) syntax()
        let λ = E(term[1])
        let argterms = term[2]
        if (argterms.length == λ.params.length) {
          let args = []
          let subscope = new Map
          for (let i = 0; i < λ.params.length; i++) {
            let arg = E(argterms[i])
            subscope.set(params[i], arg)
          }
          let x = EVAL(ctx, λ.body, subscope, λ.stack, depth + 1)
          return x
        } else {
          bad("ARGS-PARAMS-MISMATCH")
        }
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
        let λ = term[0][FUNCTION]
        if (λ) {
          let argterms = term.slice(1)
          if (argterms.length == λ.params.length) {
            let args = []
            let subscope = new Map
            for (let i = 0; i < λ.params.length; i++) {
              let arg = E(argterms[i])
              subscope.set(λ.params[i], arg)
            }
            let x = EVAL(ctx, λ.body, subscope, λ.stack, depth + 1)
            return x
          } else {
            bad("ARGS-PARAMS-MISMATCH")
          }
        } else {
          bad(`NO-DEFUN`)
        }
      }
    }
  }
  syntax()
}
  
export function stream(string) {
  let i = 0
  return {
    peek: function () {
      if (i >= string.length)
        throw new Error("eof")
      return string[i]
    },
    next: function () {
      if (i >= string.length)
        throw new Error("eof")
      return string[i++]
    }
  }
}

export function read(ctx, input) {
  skipSpaces(input)
  
  let c = input.peek()

  if (c == ')')
    throw new Error("too many r-parens")

  if (c == '(') {
    input.next()
    return readList(ctx, input)
  } 

  if (c == '"') {
    input.next()
    return readString(input)
  }

  if (c.match(/[0-9]/))
    return readNumber(input)

  if (c.match(/[-+a-zA-Z]/))
    return readSymbol(ctx, input)

  throw new Error(`unexpected character: ${c}`)
}

function skipSpaces(input) {
  while (input.peek().match(/^\s/))
    input.next()
}

function readList(ctx, input) {
  let xs = []
  for (;;) {
    skipSpaces(input)
    if (input.peek() == ")") {
      input.next()
      return xs
    }

    let x = read(ctx, input)
    xs.push(x)
  }
}

function readString(input) {
  let s = ""
  for (;;) {
    let c = input.next()
    if (c == '"') {
      return s
    }
    s += c
  }
  return s
}

function readNumber(input) {
  let s = ""
  for (;;) {
    let c = input.peek()
    if (!c.match(/[0-9]/)) {
      break
    }
    s += input.next()
  }
  return Number(s)
}

function readSymbol(ctx, input) {
  let s = ""
  for (;;) {
    let c = input.peek()
    if (!c.match(/[-a-zA-Z_+!\?0-9:]/)) {
      break
    }
    s += input.next()
  }

  if (s.match(/^([a-zA-Z-]+):(.*)$/)) {
    let packageName = RegExp.$1.toUpperCase()
    let symbolName = RegExp.$2.toUpperCase()
    if (!ctx.packages[packageName])
      throw new Error(`no package ${packageName}`)
    return ctx.packages[packageName][symbolName]
  } else if (s.match(/^([-a-zA-Z0-9\?!\+]+)$/)) {
    let symbolName = RegExp.$1.toUpperCase()
    let where = ctx.home
    for (let p of ctx.used) {
      if (p[PACKAGE_SYMBOLS].includes(symbolName)) {
        where = p
        break
      }
    }
    return where[symbolName]
  }

  throw new Error(`symbol error ${s}`)
}

export let U = PACKAGE("DEMO")

let ctx = {
  packages: {
    LISP,
    DEMO: U
  },
  used: [U, LISP],
  home: U,
}

export let example = read(ctx, stream(`
  (progn
    (defun repeat (n f)
      (if n 
        (progn 
          (apply f ())
          (repeat (- n 1) f))
        nil))
    (let ((i 0))
      (defun tick ()
        (progn
          (set! i (+ i 1))
          i)))
    (repeat 3
      (lambda ()
        (progn 
          (print "Ticking.") 
          (print (tick))))))
`))

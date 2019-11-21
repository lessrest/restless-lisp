"use strict";

export let packages = {}

export function makePackage(name) { 
  return packages[name] = {
    type: "package",
    name,
    symbols: {}
  }
}

export let lisp = makePackage("lisp")

let metasymbol = {
  type: "symbol",
  name: "symbol",
  "package": lisp,
}

metasymbol.type = metasymbol

export function intern(package_, string) {
  if (package_.symbols.hasOwnProperty(string))
    return package_.symbols[string]
  else
    return package_.symbols[string] = {
      type: metasymbol,
      "package": package_,
      name: string
    }
}

let L = lisp

function special(string) { 
  intern(lisp, string).special = 1
}

special("if")
special("let")
special("define-function")
special("do")
special("function")
special("lambda")
special("apply")
special("set!")
special("print")
special("nil")
special("+")
special("-")

let SHOW = term => {
  if (typeof term == "number")
    return term
  if (typeof term == "boolean")
    return term
  else if (typeof term == "string")
    return term
  else if (typeof term == "object") {
    if (term.type == "symbol")
      return `${term.package.name}:${term.name}`
    else if (Array.isArray(term))
      return term.map(x => SHOW(x))
  }

  throw new Error(`how to show ${term}?`)
}

let show = term => JSON.stringify(SHOW(term))

export function isSymbol(x) {
  return typeof x == "object" && x.type == metasymbol
}

export function eval_(ctx, term, scope = new Map, stack = [], depth = 0) {
  // console.log(" ".repeat(depth), show(term))
  
  function bad(x) {
    debugger
    throw new Error(`${show(intern(L, x))} (${show(term)})`) 
  }

  function syntax(x = term) {
    debugger
    throw new Error(`syntax-error (${show(x)})`)
  }
  
  function E(term, subscope = scope, substack = stack) {
    return eval_(ctx, term, subscope, substack, depth + 1)
  }

  if (typeof term == "number")
    return term
  else if (typeof term == "string")
    return term
  else if (term === L.symbols.nil)
    return term
  else if (isSymbol(term)) {
    let x = scope.get(term)
    if (x !== undefined) {
      return x
    } else {
      for (let s of stack) {
        if ((x = s.get(term)) !== undefined) return x
      }
    }
    bad("unbound-variable")
  } else if (Array.isArray(term)) {
    if (term.length > 0) {
      switch (term[0]) {
      case L.symbols["if"]: {
        if (term.length != 4) syntax()
        return E(E(term[1]) ? term[2] : term[3])
      }
        
      case L.symbols["+"]: {
        if (term.length <= 1) syntax()
        let x = 0
        for (let i = 1; i < term.length; i++)
          x += E(term[i])
        return x
      }
        
      case L.symbols["-"]: {
        if (term.length <= 2) syntax()
        let x = E(term[1])
        for (let i = 2; i < term.length; i++)
          x -= E(term[i])
        return x
      }
        
      case L.symbols["define-function"]: {
        if (term.length != 4) syntax()
        if (!isSymbol(term[1])) syntax()
        if (!Array.isArray(term[2])) syntax()
        if (term[2].some(x => !isSymbol(x))) syntax()
        return (
          term[1]["function"] = {
            type: L.symbols["function"],
            name: term[1],
            params: term[2],
            body: term[3],
            stack: [scope, ...stack],
          }
        )
      }

      case L.symbols.lambda: {
        if (term.length != 3) syntax()
        if (!Array.isArray(term[1])) syntax()
        if (term[1].some(x => !x.symbol)) syntax()
        return {
          type: L.symbols["function"],
          params: term[1],
          body: term[2],
          stack: [scope, ...stack],
        }
      }

      case L.symbols["function"]: {
        if (term.length != 2) syntax()
        if (!isSymbol(term[1])) syntax()
        if (!term[1]["function"]) bad("no-function")
        return term[1]["function"]
      }

      case L.symbols["let"]: 
        {
          if (term.length != 3) syntax()
          if (!Array.isArray(term[1])) syntax()
          let subscope = new Map
          for (let x of term[1]) {
            if (!Array.isArray(x) || x.length != 2) syntax()
            if (!isSymbol(x[0])) syntax()
            let v = E(x[1])
            subscope.set(x[0], v)
          }
          let x = eval_(ctx, term[2], subscope, [scope, ...stack], depth + 1)
          return x
        }

      case L.symbols.apply: {
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
          let x = eval_(ctx, λ.body, subscope, λ.stack, depth + 1)
          return x
        } else {
          bad("args-params-mismatch")
        }
      }

      case L.symbols.print:
        {
          if (term.length != 2) syntax()
          let x = E(term[1])
          ctx.print(x)
          return L.nil
        }
        
      case L.symbols["do"]: 
        {
          let x
          for (let subterm of term.slice(1))
            x = E(subterm)
          return x
        }
        
      case L.symbols["set!"]:
        {
          if (term.length != 3) syntax()
          if (!isSymbol(term[1])) syntax()
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
              bad("no-such-variable")
          }
          let x = E(term[2])
          where.set(term[1], x)
          return x
        }
        
      }
      
      if (isSymbol(term[0])) {
        let λ = term[0]["function"]
        if (λ) {
          let argterms = term.slice(1)
          if (argterms.length == λ.params.length) {
            let args = []
            let subscope = new Map
            for (let i = 0; i < λ.params.length; i++) {
              let arg = E(argterms[i])
              subscope.set(λ.params[i], arg)
            }
            let x = eval_(ctx, λ.body, subscope, λ.stack, depth + 1)
            return x
          } else {
            bad("args-params-mismatch")
          }
        } else {
          bad(`no-defun`)
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
    let packageName = RegExp.$1
    let symbolName = RegExp.$2
    if (!ctx.packages[packageName])
      throw new Error(`no package ${packageName}`)
    return ctx.packages[packageName][symbolName]
  } else if (s.match(/^([-a-zA-Z0-9\?!\+]+)$/)) {
    let symbolName = RegExp.$1
    let where = ctx.home
    for (let p of ctx.used) {
      if (p.symbols.hasOwnProperty(symbolName)) {
        where = p
        break
      }
    }
    return intern(where, symbolName)
  }

  throw new Error(`symbol error ${s}`)
}

export let user = makePackage("user")

let ctx = {
  packages: {
    lisp,
    user,
  },
  used: [user, lisp],
  home: user,
}

export let example = read(ctx, stream(`
  (do
    (define-function repeat (n f)
      (if n 
        (do 
          (apply f ())
          (repeat (- n 1) f))
        nil))
    (let ((counter 0))
      (define-function tick ()
        (do
          (set! counter (+ counter 1))
          counter)))
    (repeat 3
      (lambda ()
        (do 
          (print "Ticking.") 
          (print (tick)))))
    (function tick))
`))

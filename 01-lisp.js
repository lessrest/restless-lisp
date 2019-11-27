export let packages = {}

let packageType = {}
let symbolType = {}

export function makePackage(name) {
  return packages[name] = {
    type: packageType,
    name,
    symbols: {}
  }
}

export let lisp = makePackage("lisp")

packageType.type = symbolType,
packageType["package"] = lisp
packageType.name = "package"

lisp.type = packageType

symbolType.type = symbolType
symbolType.name = "symbol"
symbolType["package"] = lisp

export function intern(package_, string) {
  if (package_.symbols.hasOwnProperty(string))
    return package_.symbols[string]
  else
    return package_.symbols[string] = {
      type: symbolType,
      "package": package_,
      name: string
    }
}

let L = lisp

makePackage("keyword")

function special(string) {
  let x = intern(lisp, string)
  x.special = 1
  return x
}

export const IF = special("if")
export const LET = special("let")
export const DEFUN = special("define-function")
export const DO = special("do")
export const FUNCTION = special("function")
export const LAMBDA = special("lambda")
export const APPLY = special("apply")
export const SET = special("set!")
export const PRINT = special("print")
export const NIL = special("nil")
export const PROMPT = special("prompt")
export const CONTROL = special("control")
export const RESUME = special("resume")
export const DEFGENERIC = special("defgeneric")
export const DEFMETHOD = special("defmethod")
export const GENERIC_FUNCTION = special("generic-function")

export const STRING = intern(lisp, "string")
export const NUMBER = intern(lisp, "number")
export const ARGS = intern(lisp, "args")

export function typeOf(x) {
  if (typeof x === "string")
    return STRING
  else if (typeof x === "number")
    return NUMBER
  else {
    console.error("typeOf", x)
    throw new Error("typeOf")
  }
}

function builtin(string, params, f) {
  intern(lisp, string)["function"] = {
    type: FUNCTION,
    name: lisp.symbols[string],
    params,
    js: f,
  }
}

builtin("+", ARGS, scope => {
  let result = 0
  for (let x of scope.get(ARGS)) result += x
  return result
})

builtin("-", ARGS, scope => {
  let args = scope.get(ARGS)
  if (args.length == 0)
    throw new Error(`- needs one argument`)
  let result = args[0]
  for (let x of args.slice(1)) result -= x
  return result
})

let SHOW = term => {
  if (typeof term == "number")
    return term
  if (typeof term == "boolean")
    return term
  else if (typeof term == "string")
    return term
  else if (typeof term == "object") {
    if (term.type == symbolType)
      return `${term.package.name}:${term.name}`
    else if (Array.isArray(term))
      return term.map(x => SHOW(x))
    else if (term.type)
      return `<${term.type.name}>`
  }

  throw new Error(`how to show ${term}?`)
}

export let show = term => JSON.stringify(SHOW(term))

export function isSymbol(x) {
  return x !== null && typeof x == "object" && x.type == symbolType
}

export function stream(string) {
  let i = 0
  return {
    peek: function () {
      if (i >= string.length)
        return null
      return string[i]
    },
    next: function () {
      if (i >= string.length)
        return null
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
    if (c === null || !c.match(/[0-9]/)) {
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
    if (c === null || !c.match(/[-a-zA-Z_+!\?0-9:]/)) {
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

export let ctx = {
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

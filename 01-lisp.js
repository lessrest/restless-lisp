export const packages = {}

let packageType = {}
let symbolType = {}

export function makePackage(name) {
  return packages[name] = {
    type: packageType,
    name,
    symbols: {}
  }
}

export const lisp = makePackage("lisp")
export const user = makePackage("user")

const L = lisp

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

makePackage("keyword")

function special(string) {
  let x = intern(lisp, string)
  x.special = 1
  return x
}

export const APPLY = special("apply")
export const CONTROL = special("control")
export const DEFGENERIC = special("defgeneric")
export const DEFMETHOD = special("defmethod")
export const DEFUN = special("define-function")
export const DO = special("do")
export const FUNCTION = special("function")
export const GENERIC_FUNCTION = special("generic-function")
export const IF = special("if")
export const LAMBDA = special("lambda")
export const LET = special("let")
export const PRINT = special("print")
export const PROMPT = special("prompt")
export const RESUME = special("resume")
export const SET = special("set!")
export const QUOTE = special("quote")

export const ARGS = intern(lisp, "args")
export const NIL = intern(lisp, "nil")
export const NUMBER = intern(lisp, "number")
export const STRING = intern(lisp, "string")

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

export function isSymbol(x) {
  return x !== null && typeof x == "object" && x.type == symbolType
}

function builtin(string, params, f) {
  intern(lisp, string)["function"] = {
    type: FUNCTION,
    name: lisp.symbols[string],
    params,
    // js: f,
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

export let ctx = {
  packages: {
    lisp,
    user,
  },
  used: [user, lisp],
  home: user,
}

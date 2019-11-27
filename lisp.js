"use strict";

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

function typeOf(x) {
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

let show = term => JSON.stringify(SHOW(term))

export function isSymbol(x) {
  return x !== null && typeof x == "object" && x.type == symbolType
}

// Here is a small-step evaluation function with some kind of
// "defunctionalized continuations", or simple data structures that
// specify how to proceed with the result of a computation.
//
// We call them plans.  A plan is basically an object that describes
// a continuation.
//
// Keeping in mind that we want each step of the evaluation to be
// fully inspectable, what should the step function do?
//
// It should take an input state and produce an output state.
//
export function keval({ ctx, term, value, plan, scope, scopes }) {
//  console.log("keval", { term, value, plan })
  if (value !== undefined) {
    if (plan === null)
      return { ctx, scope, scopes, value, plan: null }

    if (plan.type == "do") {
      if (plan.terms.length != 0) {
        return {
          ctx,
          scope: plan.scope,
          scopes: plan.scopes,
          term: plan.terms[0],
          plan: {
            type: "do",
            scope: plan.scope,
            scopes: plan.scopes,
            terms: plan.terms.slice(1),
            plan: plan.plan,
          },
        }
      } else {
        return {
          ctx,
          scope: plan.scope,
          scopes: plan.scopes,
          plan: plan.plan,
          value,
        }
      }
    } else if (plan.type == "done") {
      return {
        ctx,
        scope: plan.scope,
        scopes: plan.scopes,
        value,
        plan: null,
      }
    } else if (plan.type == "print") {
      ctx.print(value)
      return {
        ctx,
        scope: plan.scope,
        scopes: plan.scopes,
        value: NIL,
        plan: plan.plan,
      }
    } else if (plan.type == "prompt-1") {
      return {
        ctx,
        scope: plan.scope,
        scopes: plan.scopes,
        term: plan.term,
        plan: {
          type: "prompt-2",
          scope: plan.scope,
          scopes: plan.scopes,
          tag: plan.tag,
          main: value,
          plan: plan.plan,
        }
      }
    } else if (plan.type == "prompt-2") {
      return {
        ctx,
        scope: new Map,
        scopes: plan.main.scopes,
        term: plan.main.body,
        plan: {
          type: "prompt-marker",
          scope: plan.scope,
          scopes: plan.scopes,
          tag: plan.tag,
          plan: plan.plan,
          handler: value,
        },
      }
    } else if (plan.type == "control") {
      /*

        So, we've been asked to perform a control operation like

          (control l v)

        and we've evaluated t and v already.

        Somewhere above us in the current continuation—in the
        transitive closure of the plan, the plan's plan, and so
        on—there should be a prompt labelled l.

        We first need to capture our current continuation as delimited
        by that prompt.  Then we need to switch to the prompt's
        continuation, with the current term and scope set to apply the
        prompt's handler to v and the captured continuation.

        We must answer two questions to specify exactly what this
        means, namely whether the captured continuation includes the
        prompt and whether the continuation switched to includes the
        prompt.

        These two questions are highlighted in "A Monadic Framework
        for Delimited Continuations" (2007) by Dybvig, Peyton Jones,
        and Sabry, and the four different answers are written as

                          -F-   -F+   +F-    +F+

        where the first -/+ means "prompt included in the continuation
        switched to" and the second -/+ means "prompt included in the
        captured continuation."

        We implement +F+.

       */

      function copy(pp, tag) {
        if (pp.type === "prompt-marker" && pp.tag === tag) {
          return [{ ...pp, plan: { type: "done" }}, pp]
        } else if (pp.type === "done") {
          throw new Error(`no prompt ${tag}`)
        } else {
          let [subcopy, prompt] = copy(pp.plan, tag)
          return [{ ...pp, plan: subcopy }, prompt]
        }
      }

      let [capture, prompt] = copy(plan.plan, plan.tag)
      return {
        ctx,
        term: prompt.handler.body,
        scopes: prompt.handler.scopes,
        scope: new Map([
          [prompt.handler.params[0], value],
          [prompt.handler.params[1], {
            type: "continuation",
            plan: capture,
          }]
        ]),
        plan: prompt.plan,
      }
    } else if (plan.type == "resume-1") {
      return {
        ctx,
        scope: plan.scope,
        scopes: plan.scopes,
        term: plan.term,
        plan: {
          type: "resume-2",
          scope: plan.scope,
          scopes: plan.scopes,
          continuation: value,
          plan: plan.plan,
        }
      }
    } else if (plan.type == "resume-2") {
      function rebase(pp, x) {
        if (pp.type === "done") {
          return x
        } else {
          return { ...pp, plan: rebase(pp.plan, x) }
        }
      }

      return {
        ctx,
        scope: plan.continuation.scope,
        scopes: plan.continuation.scopes,
        value,
        plan: rebase(plan.continuation.plan, plan.plan),
      }
    } else if (plan.type === "prompt-marker") {
      return {
        ctx,
        scope: plan.scope,
        scopes: plan.scopes,
        value,
        plan: plan.plan,
      }
    } else if (plan.type === "evaluate-funcall-arguments") {
      if (plan.terms.length === 0) {
        return funcall({
          f: plan["function"],
          args: [...plan.args, value],
          plan: plan.plan,
        })
      } else {
        return {
          ctx, scope, scopes,
          term: plan.terms[0],
          plan: {
            ...plan,
            args: [...plan.args, value],
            terms: plan.terms.slice(1),
          }
        }
      }
    }

    console.error("continue", plan, value)
    throw new Error("can't continue")
  }

  function kontinue(diff) {
    return { ctx, scope, scopes, term, plan, ...diff }
  }

  function funcall({ f, args, plan }) {
    if (f.type === GENERIC_FUNCTION) {
      for (let m of f.methods) {
        if (m.params.every(([name, type], i) => typeOf(args[i]) === type))
          return kontinue({
            scope: new Map(f.params.map((x, i) => [x, args[i]])),
            scopes: m.scopes,
            term: m.body,
            plan
          })
      }
      bad(`no-matching-method`)
    } else {
      bad(`weird-function-type: ${show(f.type)}`)
    }
  }

  if (["number", "string"].includes(typeof term)) {
    return kontinue({ value: term })
  } else if (isSymbol(term)) {
    let x = scope.get(term)
    if (x !== undefined) {
      return kontinue({ value: x })
    } else {
      for (let s of scopes) {
        if ((x = s.get(term)) !== undefined)
          return kontinue({ value: x })
      }
    }
    console.error({ term, scope, scopes })
    throw new Error("unbound-variable")
  } else if (Array.isArray(term)) {
    if (isSymbol(term[0])) {
      if (term[0] === DO) {
        return kontinue({
          term: term[1],
          plan: {
            type: "do",
            scope, scopes,
            terms: term.slice(2),
            plan,
          }
        })
      } else if (term[0] === PRINT) {
        return kontinue({
          term: term[1],
          plan: {
            type: "print",
            scope, scopes,
            plan
          }
        })
      } else if (term[0] === LAMBDA) {
        return kontinue({
          value: {
            type: L.symbols["function"],
            params: term[1],
            body: term[2],
            scopes: [scope, ...scopes],
          }
        })
      } else if (term[0] === PROMPT) {
        return kontinue({
          term: term[2],
          plan: {
            type: "prompt-1",
            scope, scopes,
            tag: term[1],
            term: term[3],
            plan
          }
        })
      } else if (term[0] === CONTROL) {
        return kontinue({
          term: term[2],
          plan: {
            type: "control",
            scope, scopes,
            tag: term[1],
            plan
          }
        })
      } else if (term[0] === RESUME) {
        return kontinue({
          term: term[1],
          plan: {
            type: "resume-1",
            scope, scopes,
            term: term[2],
            plan
          }
        })
      } else if (term[0] === DEFGENERIC) {
        syntax(term.length === 3)
        syntax(isSymbol(term[1]))
        syntax(Array.isArray(term[2]))
        syntax(term[2].every(x => isSymbol(x)))
        term[1]["function"] = {
          type: GENERIC_FUNCTION,
          name: term[1],
          params: term[2],
          methods: [],
        }
        return kontinue({
          value: term[1],
          plan
        })

      } else if (term[0] === DEFMETHOD) {
        syntax(term.length === 4)
        syntax(isSymbol(term[1]))
        syntax(Array.isArray(term[2]))
        if (term[1]["function"].type !== GENERIC_FUNCTION)
          bad("not-a-generic-function")

        term[1]["function"].methods.push({
          type: "method",
          name: term[1],
          params: term[2],
          body: term[3],
          scope, scopes
        })

        return kontinue({
          value: term[1],
          plan
        })

      } else if (isSymbol(term[0])) {
        term[0]["function"] || bad("not-a-function")

        if (term.length == 1) {
          return funcall({
            f: term[0]["function"],
            args: [],
            plan,
          })

        } else {
          return kontinue({
            term: term[1],
            plan: {
              type: "evaluate-funcall-arguments",
              "function": term[0]["function"],
              terms: term.slice(2),
              args: [],
              plan
            }
          })
        }
      }
    }
  }

  console.error("unknown term", term)
  throw new Error("unknown term")

  function bad(x) {
    debugger
    throw new Error(`${show(intern(L, x))} (${show(term)})`)
  }

  function syntax(p, x = term) {
    if (!p) {
      debugger
      throw new Error(`syntax-error ${text} (${show(x)})`)
    }
  }
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

export function execute({
  ctx, term, limit = 100,
}) {
  let i = 0
  let output = []
  let s = {
    ctx,
    term,
    plan: { type: "done" },
    scope: new Map,
    scopes: [],
  }

  for (;;) {
    if (i++ > limit)
      return s
    if ((s = keval(s)).plan === null)
      return s
  }
}

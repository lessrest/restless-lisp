import {
  CONTROL,
  DEFGENERIC,
  DEFMETHOD,
  DEFUN,
  DO,
  FUNCTION,
  GENERIC_FUNCTION,
  LAMBDA,
  NIL,
  PRINT,
  PROMPT,
  RESUME,
  QUOTE,

  isSymbol,
  typeOf,
  show,
  intern,
  lisp,
} from "./01-lisp.js"

// Here is a small-step evaluation function with some kind of
// "defunctionalized continuations", or simple data structures
// ("plans") that specify how to proceed with the result of
// a computation.
//
export function keval({ ctx, term, value, plan, scopes }) {

  if (value !== undefined) {
    // If we have a value, then the term has been fully evaluated.
    // We should carry out the current plan, and move on.

    if (plan === null)
      return { ctx, scopes, value, plan: null }

    if (plan.type == "do") {
      if (plan.terms.length != 0) {
        return {
          ctx,
          scopes: plan.scopes,
          term: plan.terms[0],
          plan: {
            type: "do",
            scopes: plan.scopes,
            terms: plan.terms.slice(1),
            plan: plan.plan,
          },
        }
      } else {
        return {
          ctx,
          scopes: plan.scopes,
          plan: plan.plan,
          value,
        }
      }

    } else if (plan.type == "done") {
      return {
        ctx,
        scopes: plan.scopes,
        value,
        plan: null,
      }

    } else if (plan.type == "print") {
      ctx.print(value)
      return {
        ctx,
        scopes: plan.scopes,
        value: NIL,
        plan: plan.plan,
      }

    } else if (plan.type == "prompt-1") {
      return {
        ctx,
        scopes: plan.scopes,
        term: plan.term,
        plan: {
          type: "prompt-2",
          scopes: plan.scopes,
          tag: plan.tag,
          main: value,
          plan: plan.plan,
        }
      }

    } else if (plan.type == "prompt-2") {
      return {
        ctx,
        scopes: [new Map, ...plan.main.scopes],
        term: plan.main.body,
        plan: {
          type: "prompt-marker",
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
        scopes: [
          new Map([
            [prompt.handler.params[0], value],
            [prompt.handler.params[1], {
              type: "continuation",
              plan: capture,
            }]
          ]),
          ...prompt.handler.scopes,
        ],
        plan: prompt.plan,
      }

    } else if (plan.type == "resume-1") {
      return {
        ctx,
        scopes: plan.scopes,
        term: plan.term,
        plan: {
          type: "resume-2",
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
        scopes: plan.continuation.scopes,
        value,
        plan: rebase(plan.continuation.plan, plan.plan),
      }

    } else if (plan.type === "prompt-marker") {
      return {
        ctx,
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
          ctx, scopes,
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

  // We don't have a value, so we inspect the term and choose a plan.

  function kontinue(diff) {
    return { ctx, scopes, term, plan, ...diff }
  }

  function bad(x) {
    debugger
    throw new Error(`${show(intern(lisp, x))} (${show(term)})`)
  }

  function syntax(p, x = term) {
    if (!p) {
      debugger
      throw new Error(`syntax-error ${text} (${show(x)})`)
    }
  }

  function funcall({ f, args, plan }) {
    if (f.type === GENERIC_FUNCTION) {
      for (let m of f.methods) {
        if (m.params.every(([name, type], i) => typeOf(args[i]) === type))
          return kontinue({
            scopes: [
              new Map(f.params.map((x, i) => [x, args[i]])),
              m.scopes,
            ],
            term: m.body,
            plan
          })
      }
      bad(`no-matching-method`)
    }

    else if (f.type === FUNCTION) {
      return kontinue({
        scopes: [
          new Map(f.params.map((x, i) => [x, args[i]])),
          f.scopes,
        ],
        term: f.body,
        plan,
      })
    }

    else {
      bad(`weird-function-type: ${show(f.type)}`)
    }
  }

  // Self-evaluating literal?
  if (["number", "string"].includes(typeof term)) {
    return kontinue({ value: term })
  }

  // Variable reference?
  else if (isSymbol(term)) {
    let x
    for (let s of scopes)
      if ((x = s.get(term)) !== undefined)
        return kontinue({ value: x })
    console.error({ term, scopes })
    throw new Error("unbound-variable")
  }

  // Special form or function call?
  else if (Array.isArray(term)) {

    if (isSymbol(term[0])) {
      if (term[0] === QUOTE) {
        return kontinue({
          value: term[1],
          plan
        })
      }

      else if (term[0] === DO) {
        return kontinue({
          term: term[1],
          plan: {
            type: "do",
            scopes,
            terms: term.slice(2),
            plan,
          }
        })
      }

      else if (term[0] === PRINT) {
        return kontinue({
          term: term[1],
          plan: {
            type: "print",
            scopes,
            plan
          }
        })
      }

      else if (term[0] === LAMBDA) {
        return kontinue({
          value: {
            type: FUNCTION,
            params: term[1],
            body: term[2],
            scopes,
          }
        })
      }

      else if (term[0] === PROMPT) {
        return kontinue({
          term: term[2],
          plan: {
            type: "prompt-1",
            scopes,
            tag: term[1],
            term: term[3],
            plan
          }
        })
      }

      else if (term[0] === CONTROL) {
        return kontinue({
          term: term[2],
          plan: {
            type: "control",
            scopes,
            tag: term[1],
            plan
          }
        })
      }

      else if (term[0] === RESUME) {
        return kontinue({
          term: term[1],
          plan: {
            type: "resume-1",
            scopes,
            term: term[2],
            plan
          }
        })
      }

      else if (term[0] === DEFGENERIC) {
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
      }

      else if (term[0] === DEFMETHOD) {
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
          scopes
        })

        return kontinue({
          value: term[1],
          plan
        })
      }

      else if (term[0] === DEFUN) {
        syntax(term.length === 4)
        syntax(isSymbol(term[1]))
        syntax(Array.isArray(term[2]))

        term[1]["function"] = {
          type: FUNCTION,
          name: term[1],
          params: term[2],
          body: term[3],
          scopes
        }

        return kontinue({
          value: term[1],
          plan
        })
      }

      else if (isSymbol(term[0])) {
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

    bad(`cannot apply non-symbol`)
  }

  console.error("unknown term", term)
  throw new Error("unknown term")
}

export function execute({
  ctx, term, limit = 100,
}) {
  let i = 0
  let output = []
  let s = {
    ctx,
    term,
    plan: { type: "done" },
    scopes: [],
  }

  for (;;) {
    if (i++ > limit)
      return s
    if ((s = keval(s)).plan === null)
      return s
  }
}

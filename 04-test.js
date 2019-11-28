import { packages, NIL } 
  from "./01-lisp.js"
import { keval } 
  from "./02-keval.js"
import { readFromString }
  from "./03-read.js"

import { deepEqual } 
  from "assert"

let { user, lisp } = packages

let defaultCtx = {
  packages: {
    lisp,
    user,
  },
  used: [user, lisp],
  home: user,
}

function testProgram({
  name,
  ctx = defaultCtx,
  code,
  expect,
  limit = 100,
}) {
  process.stdout.write(`${name}: `)

  let term = readFromString(ctx, code)
  let i = 0
  let output = []
  let s = {
    ctx: { ...ctx, print: x => output.push(x) },
    term,
    plan: { type: "done" },
    scope: new Map,
    scopes: [],
  }

  for (;;) {
    if (i++ > limit)
      throw new Error("keval took too many steps")
    if ((s = keval(s)).plan === null)
      break
  }

  deepEqual(expect.output, output)
  deepEqual(expect.result, s.value)

  process.stdout.write(`ok\n`)
}

export let tests = [
  {
    name: "prompt with multiple resumptions",
    code: `
      (do
        (print
          (prompt 0
            (lambda ()
              (do (print (control 0 "escaped"))
                  (print "returned")
                  "done A"))
            (lambda (v k)
              (do (print "controlled")
                  (print v)
                  (resume k "restart 1")
                  (resume k "restart 2")))))
        (print "ok"))
    `,
    expect: {
      output: [
        "controlled",
        "escaped",
        "restart 1",
        "returned",
        "restart 2",
        "returned",
        "done A",
        "ok",
      ],
      result: NIL,
    },
  },

  {
    name: "defgeneric/defmethod",
    code: `
      (do
        (defgeneric foo (x))
        (defmethod foo ((x number))
          (do
            (print "number")
            (print 1)))
        (defmethod foo ((x string))
          (do
            (print "string")
            (print x)))
        (foo 1)
        (foo "hey"))
    `,
    expect: {
      output: ["number", 1, "string", "hey"],
      result: NIL,
    },
  },
]

export let example = readFromString(defaultCtx, `
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
`)

for (let test of tests) {
  testProgram(test)
}

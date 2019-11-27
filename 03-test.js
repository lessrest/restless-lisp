import { read, stream, packages, NIL } 
  from "./01-lisp.js"
import { keval } 
  from "./02-keval.js"

import { deepEqual } 
  from 'assert'

let { user, lisp } = packages

function testProgram({
  name,
  ctx = {
    home: user,
    used: [user, lisp],
  },
  code,
  expect,
  limit = 100,
}) {
  process.stdout.write(`${name}: `)

  let term = read(ctx, stream(code))
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

for (let test of tests) {
  // if (test.name === "defgeneric/defmethod") {
    testProgram(test)
  // }
}

import { example, eval_, keval, read, stream, ctx } from "./lisp.js"
// eval_({
//   print: x => console.log(x),
// }, example)

// let test = read(ctx, stream(`
//   (do (print 1) (print 2) (print 3))
// `))

let test = read(ctx, stream(`
  (do
    (print
      (prompt 0
        (lambda ()
          (do (control 0 "hey")
              (print "unreachable")))
        (lambda (v k)
          (do (print "controlled")
              (print v)
              (resume k "restart")))))
    (print "ok"))
`))

let state = {
  ctx: {
    ...ctx,
    print: x => console.log(x),
  },
  term: test,
  plan: { type: "done" },
  scope: new Map,
  scopes: [],
  prompts: [],
}

for (let i = 0; i < 100; i++) {
  state = keval(state)
  if (state.plan === null) {
    console.log("Exited")
    console.log(state)
    break
  }
}


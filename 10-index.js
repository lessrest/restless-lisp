import {
  packages,
  isSymbol,
  makePackage,
  LAMBDA,
} from "./01-lisp.js"

import { keval }
  from "./02-keval.js"
import { readFromString }
  from "./03-read.js"
import { html }
  from "./11-html.js"
import Context
  from "./12-react-context.js"
import { Repl }
  from "./31-repl.js"
import { Debugger }
  from "./32-debugger.js"
import { Buffer }
  from "./40-buffer.js"

import { render, h }
  from "./vendor/preact.js"

window.packages = packages

function bar () {
  let output = []
  let ctx = {
    home: packages.user,
    used: [packages.user, packages.lisp],
    print: x => output.push(h("div", {}, ["output: ", x]))
  }

  let test = readFromString(ctx,`
    (do
      (print
        (prompt 0
          (lambda ()
            (do (print (control 0 "escaped"))
                (print "returned")))
          (lambda (v k)
            (do (print "controlled")
                (print v)
                (resume k "restart 1")
                (resume k "restart 2")))))
      (print "ok"))
  `)

  let expr = html`
    <${Context.Provider} value=${ctx}>
      <${Term} term=${test}/>
    </>`
  render(expr, document.body)

  let s = [{
    term: test,
    plan: { type: "done" },
    scope: new Map,
    scopes: [],
    prompts: [],
  }]

  ctx.print = x => {
    console.info("output", x)
    s.push(["output", x])
  }

  let i = 0
  while (s[s.length - 1].plan && (i++ < 100)) {
    console.log(s)
    s.push(keval(s[s.length - 1]))
    output = []
  }

  render(
    html`
     <${Context.Provider} value=${ctx}>
       <div>
         <div>
           ${expr}
         </div>
         <ol>
           ${s.map(x => h("li", {}, h(Term, { term: x })))}
         </ol>
       </div>
     </>`,
    document.body)
}

function baz() {
  let ctx = {
    home: packages.user,
    used: [packages.user, packages.lisp],
  }

  let test = readFromString(ctx, `
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
  `)

  let test2 = readFromString(ctx, `
    (do
      (defgeneric foo (x))
      (defmethod foo ((x number))
        (do
          (print "number")
          (print 1)))
      (foo 1))
  `)

  let s = {
    ctx,
    term: test,
    plan: { type: "done" },
    scope: new Map,
    scopes: [],
  }

  render(h(Debugger, { machine: s }), document.body)
}

onload = baz

let repl_onload = () => {
  render(h(Repl, {
    history: [
      {
        note: h("header", {}, [
          h("img", {
            src: "img/lusen02b.svg",
            style: `width: 8rem; align-self: center `
          }) ,
          html`<p>Welcome to Restless Lisp.<br/></p>`,
          html`<br/>`
        ])
      }
    ]
  }), document.body)
}

let xxxonload = () => {
  let path = [0]

  function draw() {
    render(h(Buffer, {
      values: [["Hello,", "world."], ["What's up?"]],
      path,
    }), document.body)
  }

  window.onkeypress = e => {
    console.log("key", e)
    if (e.key === "n") {
      path[path.length - 1]++
    }

    else if (e.key === "p") {
      path[path.length - 1]--
    }

    draw()
  }

  draw()
}

// onload = () => baz()

// (prompt 0 (lambda () (control 0 "x")) (lambda (x k) k))

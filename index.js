import {
  packages,
  isSymbol,
  makePackage,
  keval,
  example,
  read,
  stream
} from './lisp.js'

// import { tests } from './test.js'

import htm
  from './htm.js'
import {
  Component, render, createContext, h
} from './preact.js'

let html = htm.bind(h)

window.packages = packages

let Context = createContext("lisp")

class Term extends Component {
  constructor(props) {
    super(props)
    this.state = {
      expanded: !!props.expanded
    }
  }

  expand = e => {
    e.stopPropagation()
    console.log("click", this.state.expanded)
    this.setState({ expanded: !this.state.expanded })
  }

  render(props, state) {
    let term = props.term
    let point = props.point
    let pointy = point === term ? { "data-point": "yes" } : {}

    let renderFields = () => {
      let rows = []
      for (let [k, v] of Object.entries(term)) {
        rows.push(html`
        <tr>
          <td>${k}</td>
          <td><${Term} term=${v} point=${point}/></td>
        </tr>
       `)
      }
      return html`<table>${rows}</table>`
    }

    if (term === null)
      return h("lisp-null", { ...pointy }, h("b", {}, "null"))
    if (typeof term == "number")
      return h("lisp-number", { ...pointy }, term)
    else if (typeof term == "string")
      return h("lisp-string", { ...pointy }, term)
    else if (typeof term == "function")
      return h("lisp-native", { ...pointy }, "JavaScript function")
    else if (term instanceof Map) {
      let rows = []
      for (let [k, v] of term) {
        rows.push(h("tr", {}, [
          h("td", {}, h(Term, { term: k, point })),
          h("td", {}, h(Term, { term: v, point })),
        ]))
      }
      return html`<lisp-env><table>${rows}</table></lisp-env>`
    }
    else if (isSymbol(term)) {
      return h(
        Context.Consumer, {},
        context => {
          let attrs = { onclick: this.expand }
          if (term.special) attrs["class"] = "special"

          let name
          if (context.used.includes(term.package))
            name = term.name
          else
            name = `${term.package.name}:${term.name}`

          if (state.expanded) console.log("yep")

          return h(
            "lisp-symbol", { ...pointy, ...attrs },
            state.expanded ? renderFields() : name
          )
        }
      )
    } else if (Array.isArray(term)) {
      if (term.length) {
        let attrs
        if (isSymbol(term[0])) {
          let name = `${term[0]["package"].name}:${term[0].name}`
          attrs = { "data-call": name }
        }
        return html`
         <lisp-list ...${pointy} ...${attrs}>
           ${term.map(x => html`<${Term} term=${x} point=${point}/>`)}
         </lisp-list>
       `
      } else
        return html`<lisp-list class=empty ...${pointy}></lisp-list>`
    } else if (typeof term == "object" && term.type) {
      if (state.expanded) {
        return html`
         <lisp-object ...${pointy}>${renderFields()}</lisp-object>
       `
      } else {
        return html`
        <lisp-object onclick=${this.expand} ...${pointy}><i>object</i></lisp-object>
       `
      }
    } else if (typeof term == "object") {
      if (state.expanded) {
        return html`
         <lisp-object ...${pointy}>${renderFields()}</lisp-object>
       `
      } else {
        return html`
          <lisp-object ...${pointy} onclick=${this.expand}><i>object</i></lisp-object>
         `
      }
    } else if (term === undefined) {
        return html`
          <lisp-undefined ...${pointy}><b>undefined</b></lisp-undefined>
         `
    } else
      throw new Error(`unknown thing: ${term}`)
  }
}

function bar () {
  let output = []
  let ctx = {
    home: packages.user,
    used: [packages.user, packages.lisp],
    print: x => output.push(h("div", {}, ["output: ", x]))
  }

  let test = read(ctx, stream(`
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
  `))

  let expr = html`
    <${Context.Provider} value=${ctx}>
      <${Term} term=${test}/>
    </>`
  render(expr, document.body)

  let s = [{
    ctx,
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

class Debugger extends Component {
  constructor(props) {
    super(props)
    this.state = {
      output: [],
      program: props.machine.term,
      machine: props.machine,
    }
  }

  step = () => {
    let output = []
    this.setState({
      machine: keval({
        ...this.state.machine,
        ctx: {
          ...this.state.machine.ctx,
          print: x => output.push(x),
        },
      })
    }, () => {
      if (output.length) {
        this.setState({ output: [...this.state.output, ...output] })
      }
    })
  }

  render(props, state) {
    return h(Context.Provider, { value: state.machine.ctx }, html`
      <lisp-debugger>
        <button onclick=${this.step}>Step</button>
        <table>
          <tr>
            <td>program</td><td>${h(Term, { term: state.program, point: state.machine.term })}</td>
          </tr>
          <tr>
            <td>output</td><td>${h(Term, { term: state.output })}</td>
          </tr>
          <tr>
            <td>term</td><td>${h(Term, { term: state.machine.term })}</td>
          </tr>
          <tr>
            <td>value</td><td>${h(Term, { term: state.machine.value, expanded: true })}</td>
          </tr>
          <tr>
            <td>plan</td><td>${h(Term, { term: state.machine.plan, expanded: true })}</td>
          </tr>
          <tr>
            <td>scope</td><td>${h(Term, { term: state.machine.scope })}</td>
          </tr>
          <tr>
            <td>scopes</td><td>${h(Term, { term: state.machine.scopes })}</td>
          </tr>
        </table>
      </lisp-debugger>
    `)
  }
}

function baz() {
  let ctx = {
    home: packages.user,
    used: [packages.user, packages.lisp],
  }

  let test = read(ctx, stream(`
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
  `))

  let test2 = read(ctx, stream(`
    (do
      (defgeneric foo (x))
      (defmethod foo ((x number))
        (do
          (print "number")
          (print 1)))
      (foo 1))
  `))

  let s = {
    ctx,
    term: test2,
    plan: { type: "done" },
    scope: new Map,
    scopes: [],
  }

  render(h(Debugger, { machine: s }), document.body)
}

function execute({
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

class Repl extends Component {
  constructor(props) {
    super(props)
    this.state = {
      history: props.history,
      input: "",
      ctx: {
        home: packages.user,
        used: [packages.user, packages.lisp],
        print: x => this.append({ output: x }),
      }
    }
  }

  append = x => setTimeout(() => {
    console.log("print", x)
    this.setState({
      history: [...this.state.history, x]
    })
  })

  setInput = e => this.setState({ input: e.target.value })
  submit = e => {
    e.preventDefault()
    this.setState({ input: "" })
    let term = read(this.state.ctx, stream(this.state.input))

    this.append({ input: term })

    console.log(this.state)

    try {
      let s = execute({
        ctx: this.state.ctx,
        term,
        limit: 100,
      })

      if (s.plan === null) {
        this.append({ result: s.value })
      } else {
        this.append({ timeout: true })
      }
    } catch (e) {
      this.append({ error: e })
    }
  }

  render(props, state) {
    function History(x) {
      if (x.output)
        return h("lisp-output", {}, h(Term, { term: x.output, expanded: true }))
      else if (x.input)
        return h("lisp-input", {}, h(Term, { term: x.input, expanded: true }))
      else if (x.result)
        return h("lisp-result", {}, h(Term, { term: x.result, expanded: true }))
      else if (x.error)
        return h("lisp-error", {}, h(Term, { term: x.error.stack, expanded: true }))
      else if (x.note)
        return h("i", {}, x.note)
      else
        throw new Error(x)
    }

    return h(Context.Provider, { value: this.state.ctx }, [
      h("lisp-repl", {}, [
        h("lisp-history", {}, state.history.map(x => History(x))),
        h("form", {
          onsubmit: e => this.submit(e)
        }, h("input", {
          placeholder: "Lisp term...",
          autofocus: true,
          value: this.state.input,
          onchange: e => this.setInput(e),
        })),
      ])
    ])
  }
}

onload = () => {
  render(h(Repl, {
    history: [
      {
        note: h("header", {}, [
          h("img", {
            src: "img/lusen02b.svg",
            style: `width: 14vw; align-self: center `
          }) ,
          html`<p>Welcome to Restless Lisp.<br/></p>`,
          html`<br/>`
        ])
      }
    ]
  }), document.body)
}

// onload = () => baz()

// (prompt 0 (lambda () (control 0 "x")) (lambda (x k) k))

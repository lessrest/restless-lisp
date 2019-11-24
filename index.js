import { 
  packages, 
  isSymbol, 
  makePackage, 
  eval_,
  keval,
  example,
  read,
  stream
} from './lisp.js'

import htm 
  from './htm.js'
import { 
  Component, render, createContext, h 
} from './preact.js'

let html = htm.bind(h)

window.packages = packages

let Context = createContext("lisp")

class Term extends Component {
  expand = () => {
    this.setState({ expanded: true })
  }

  render(props, state) {
    let term = props.term

    let renderFields = () => {
      let rows = []
      for (let [k, v] of Object.entries(term)) {
        rows.push(html`
        <tr>
          <td>${k}</td>
          <td><${Term} term=${v}/></td>
        </tr>
       `)
      }
      return html`<table>${rows}</table>`
    }

    if (term === null)
      return h("lisp-null", {}, h("b", {}, "null"))
    if (typeof term == "number")
      return h("lisp-number", {}, term)
    else if (typeof term == "string")
      return h("lisp-string", {}, term)
    else if (typeof term == "function")
      return h("lisp-native", {}, "JavaScript function")
    else if (term instanceof Map) {
      let rows = []
      for (let [k, v] of term) {
        rows.push(h("tr", {}, [
          h("td", {}, h(Term, { term: k })),
          h("td", {}, h(Term, { term: v })),
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
            "lisp-symbol", attrs, 
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
         <lisp-list ...${attrs}>
           ${term.map(x => html`<${Term} term=${x}/>`)}
         </lisp-list>
       `
      } else 
        return html`<lisp-list class=empty></lisp-list>`
    } else if (typeof term == "object" && term.type) {
      if (state.expanded) {
        return html`
         <lisp-object>${renderFields()}</lisp-object>
       `
      } else {
        return html`
        <lisp-object onclick=${this.expand}><i>object</i></lisp-object>
       `
      } 
    } else if (typeof term == "object") {
      if (state.expanded) {
        return html`
         <lisp-object>${renderFields()}</lisp-object>
       `
      } else {
        return html`
          <lisp-object onclick=${this.expand}><i>object</i></lisp-object>
         `
      } 
    } else if (term === undefined) {
        return html`
          <lisp-undefined><b>undefined</b></lisp-undefined>
         `
    } else
      throw new Error(`unknown thing: ${term}`)
  }
}

function foo () {
  let output = []
  let ctx = {
    used: [packages.user, packages.lisp],
    print: x => output.push(x)
  }

  let expr = html`
    <${Context.Provider} value=${ctx}>
      <${Term} term=${example}/>
    </>`
  render(expr, document.body)

  let value = eval_(ctx, example)
  render(
    html`
     <${Context.Provider} value=${ctx}>
       <div>
         <div>
           ${expr}
         </div>
         <div>
           Output was <${Term} term=${output}/>.
         </div>
         <div>
           Result was <${Term} term=${value}/>.
         </div>
       </div>
     </>`, 
    document.body)
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
            <td>program</td><td>${h(Term, { term: state.program })}</td>
          </tr>
          <tr>
            <td>output</td><td>${h(Term, { term: state.output })}</td>
          </tr>
          <tr>
            <td>term</td><td>${h(Term, { term: state.machine.term })}</td>
          </tr>
          <tr>
            <td>plan</td><td>${h(Term, { term: state.machine.plan })}</td>
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
                (print "returned")))
          (lambda (v k)
            (do (print "controlled")
                (print v)
                (resume k "restart 1")
                (resume k "restart 2")))))
      (print "ok"))
  `))

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

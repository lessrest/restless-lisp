import { packages, isSymbol, makePackage, eval_, example } from './lisp.js'
import { Component, html, render } from 'https://unpkg.com/htm/preact/standalone.module.js'

window.packages = packages

class Term extends Component {
  render(props, state) {
    let term = props.term
    let expand = () => this.setState({ expanded: !state.expanded })

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

    if (typeof term == "number")
      return html`<lisp-number>${term}</lisp-number>`
    else if (typeof term == "string")
      return html`<lisp-string>${term}</lisp-string>`
    else if (term instanceof Map) {
      let rows = []
      for (let [k, v] of term) {
        rows.push(html`
        <tr>
          <td><${Term} term=${k}/></td>
          <td><${Term} term=${v}/></td>
        </tr>
       `)
      }
      return html`<lisp-env><table>${rows}</table></lisp-env>`
    }
    else if (isSymbol(term)) {
      console.log(term)
      let attrs = term.special ? { "class": "special" } : {}
      let name = `${term.package.name}:${term.name}`
      if (state.expanded) {
        return html`
        <lisp-symbol ...${attrs}>${renderFields()}</lisp-symbol>
      `
      } else
        return html`
        <lisp-symbol ...${attrs} onclick=${expand}>${name}</lisp-symbol>
        `
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
        <lisp-object onclick=${expand}><i>object</i></lisp-object>
       `
      } 
    } else if (typeof term == "object") {
      if (state.expanded) {
        return html`
         <lisp-object>${renderFields()}</lisp-object>
       `
      } else {
        return html`
        <lisp-object onclick=${expand}><i>fields</i></lisp-object>
       `
      } 
    } else
      throw new Error(`unknown thing: ${term}`)
  }
}

let show = (term, opts = {}) => {
  let {
    used = [packages.lisp, packages.user], 
    verbose = false 
  } = opts

  if (typeof term == "number")
    return html`<lisp-number>${term}</lisp-number>`
  else if (typeof term == "string")
    return html`<lisp-string>${term}</lisp-string>`
  else if (isSymbol(term)) {
    let attrs = term.special ? { "class": "special" } : {}
    let name
    if (!verbose && used.includes(term["package"]))
      name = term.name
    else
      name = `${term.package.name}:${term.name}`
    return html`<lisp-symbol ...${attrs}>${name}</lisp-symbol>`
  } else if (Array.isArray(term)) {
    if (term.length) {
      let attrs
      if (isSymbol(term[0])) {
        let name = `${term[0]["package"].name}:${term[0].name}`
        attrs = { "data-call": name }
      }
      return html`
       <lisp-list ...${attrs}>
         ${term.map(x => show(x, opts))}
       </lisp-list>
     `
    } else 
      return html`<lisp-list class=empty></lisp-list>`
  } else if (typeof term == "object" && term.type == packages.lisp.symbols["function"]) {
    return html`
     <lisp-function>
       <table>
         <tr><td>type</td><td>${show(term.type, opts)}</td></tr>
         <tr><td>name</td><td>${term.name ? show(term.name, opts) : "—"}</td></tr>
       </table>
     </lisp-function>
   `
  } else
    throw new Error(`unknown thing: ${term}`)
}

onload = () => {
  let expr = html`<${Term} term=${example}/>`
  render(expr, document.body)
  let output = []
  let ctx = {
    print: x => output.push(x)
  }
  let value = eval_(ctx, example)
  render(
    html`
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
     </div>`, 
    document.body)
}

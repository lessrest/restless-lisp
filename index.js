import { 
  packages, 
  isSymbol, 
  makePackage, 
  eval_, 
  example 
} from './lisp.js'

import htm 
  from 'https://unpkg.com/htm?module'
import { 
  Component, render, createContext, h 
} from 'https://cdn.jsdelivr.net/npm/preact@10.0.5/dist/preact.module.js'

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
        <lisp-object onclick=${this.expand}><i>fields</i></lisp-object>
       `
      } 
    } else
      throw new Error(`unknown thing: ${term}`)
  }
}

onload = () => {
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

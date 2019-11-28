import { isSymbol }
  from "./01-lisp.js"
import { html }
  from "./11-html.js"
import Context
  from "./12-react-context.js"

import { Component, h }
  from "./vendor/preact.js"

export class Term extends Component {
  constructor(props) {
    super(props)
    this.state = {
      expanded: !!props.expanded
    }
  }

  expand = e => {
    e.stopPropagation()
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
            name = term.alias || term.name
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

import { SYMBOL, SPECIAL, LISP, PACKAGE, PACKAGE_NAME, EVAL, USER, example } from './lisp.js'
import { html, render } from 'https://unpkg.com/htm/preact/standalone.module.js'

let L = LISP
let U = USER

let show = (term, used = [L, U]) => {
  if (typeof term == "number")
    return html`<lisp-number>${term}</lisp-number>`
  else if (typeof term == "string")
    return html`<lisp-string>${term}</lisp-string>`
  else if (term[SYMBOL]) {
    let attrs = term[SPECIAL] ? { "class": "special" } : {}
    let name
    if (used.includes(term[SYMBOL].PACKAGE))
      name = term[SYMBOL].NAME
    else
      name = `${term[SYMBOL].PACKAGE[PACKAGE_NAME]}:${term[SYMBOL].NAME}`
    return html`<lisp-symbol ...${attrs}>${name}</lisp-symbol>`
  } else if (Array.isArray(term)) {
    if (term.length) {
      let attrs
      if (term[0][SYMBOL]) {
        let x = term[0][SYMBOL]
        let name = `${x.PACKAGE[PACKAGE_NAME]}:${x.NAME}`
        attrs = { "data-call": name }
      }
      return html`<lisp-list ...${attrs}>${term.map(x => show(x))}</lisp-list>`
    } else 
      return html`<lisp-list class=empty></lisp-list>`
  } else
    throw new Error(`unknown thing: ${term}`)
}

onload = () => {
  let expr = show(example)
  render(expr, document.body)
  let output = []
  let ctx = {
    print: x => output.push(x)
  }
  let value = EVAL(ctx, example)
  render(
    html`
     <div>
       <div>
         ${expr}
       </div>
       <div>
         Output was ${show(output)}.
       </div>
       <div>
         Result was ${show(value)}.
       </div>
     </div>`, 
    document.body)
}

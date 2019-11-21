import { SYMBOL, SPECIAL, LISP, PACKAGE, PACKAGE_NAME, EVAL, U, example } from './elli.js'
import { html, render } from 'https://unpkg.com/htm/preact/standalone.module.js'

let L = LISP
// let U = PACKAGE("USER")

// let example = (
//   [L.PROGN,
//    [L.DEFUN, U.FOO, [U.X, U.Y],
//     [L["+"], U.X, U.Y, 1]],
//    [U.FOO, 2, 2]]
// )

let show = (term, used = [L, U]) => {
  if (typeof term == "number")
    return html`<elli-number>${term}</elli-number>`
  else if (typeof term == "string")
    return html`<elli-string>${term}</elli-string>`
  else if (term[SYMBOL]) {
    let attrs = term[SPECIAL] ? { "class": "special" } : {}
    let name
    if (used.includes(term[SYMBOL].PACKAGE))
      name = term[SYMBOL].NAME
    else
      name = `${term[SYMBOL].PACKAGE[PACKAGE_NAME]}:${term[SYMBOL].NAME}`
    return html`<elli-symbol ...${attrs}>${name}</elli-symbol>`
  } else if (Array.isArray(term)) {
    if (term.length) {
      let attrs
      if (term[0][SYMBOL]) {
        let x = term[0][SYMBOL]
        let name = `${x.PACKAGE[PACKAGE_NAME]}:${x.NAME}`
        attrs = { "data-call": name }
      }
      return html`<elli-list ...${attrs}>${term.map(x => show(x))}</elli-list>`
    } else 
      return html`<elli-list class=empty></elli-list>`
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
  console.log(output)
  render(
    html`
     <div>
       ${expr}
       <div>
         Output: ${show(output)}
       </div>
       <div>
         Result: ${show(value)}
       </div>
     </div>`, 
    document.body)
}

import { isSymbol, ctx }
  from "./01-lisp.js"
import { h }
  from "./11-html.js"

export function elli(node, f) {
  f(node)
  return node
}

function channel() {
  let readers = []
  return {
    send: x => {
      for (let reader of readers)
        reader(x)
      readers = []
    },
    next: () => {
      return new Promise((ok, no) => readers.push(ok))
    },
  }
}

function paint(node, ...children) {
  while (node.hasChildNodes())
    node.removeChild(node.lastChild)
  for (let x of children) {
    node.appendChild(x)
  }
}

export function draw(ctx, term) {
  function expandable(node, x) {
    return elli(
      node,
      async self => {
        let click = channel()
        let expanded = false

        self.onclick = e => {
          e.stopPropagation()
          click.send(true)
          return false
        }

        for (;;) {
          paint(self, x(expanded))
          await click.next()
          expanded = !expanded
        }
      }
    )
  }

  function renderFields() {
    let rows = []
    for (let [k, v] of Object.entries(term)) {
      rows.push(
        h("tr", {}, [
          h("td", {}, k),
          h("td", {}, draw(ctx, v)),
        ]))
    }
    return h("table", {}, rows)
  }

  if (term === null)
    return h("lisp-null", { }, h("b", {}, "null"))

  if (typeof term == "number")
    return h("lisp-number", { }, `${term}`)

  else if (typeof term == "string")
    return h("lisp-string", { }, term)

  else if (typeof term == "function")
    return h("lisp-native", { }, "JavaScript function")

  else if (term instanceof Map) {
    let rows = []
    for (let [k, v] of term) {
      rows.push(h("tr", {}, [
        h("td", {}, draw(ctx, k)),
        h("td", {}, draw(ctx, v)),
      ]))
    }
    return h("lisp-env", {}, h("table", {}, rows))
  }

  else if (isSymbol(term)) {
    let name
    if (ctx.used.includes(term.package))
      name = term.alias || term.name
    else
      name = `${term.package.name}:${term.name}`

    return expandable(
      h("lisp-symbol"),
      expanded => expanded ? renderFields() : h("span", {}, name)
    )

  } else if (Array.isArray(term)) {
    if (term.length) {
      let attrs
      if (isSymbol(term[0])) {
        let name = `${term[0]["package"].name}:${term[0].name}`
        attrs = { "data-call": name }
      }

      return h(
        "lisp-list", { ...attrs },
        term.map(x => draw(ctx, x)))
    } else {
      return h("lisp-list", { "class": "empty" })
    }

  } else if (typeof term == "object" && term.type) {
    return expandable(
      h("lisp-object"),
      expanded => expanded ? renderFields() : h("i", {}, "object")
    )

  } else if (typeof term == "object") {
    return expandable(
      h("lisp-object"),
      expanded => expanded ? renderFields() : h("i", {}, "object")
    )

  } else if (term === undefined) {
    return h("lisp-undefined", {}, "undefined")

  } else
    throw new Error(`unknown thing: ${term}`)
}

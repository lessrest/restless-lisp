import { packages }
  from "./01-lisp.js"
import { execute }
  from "./02-keval.js"
import { readFromString }
  from "./03-read.js"
import Context
  from "./12-react-context.js"
import { Term }
  from "./30-term.js"

import { Component, h }
  from "./vendor/preact.js"

export class Buffer extends Component {
  constructor(props) {
    super(props)
  }

  render(props, state) {
    let children = props.values.map(x => h(Term, { term: x }))
    if (props.path.length === 1) {
      let i = props.path[0]
      if (i < 0)
        i = 0
      children.splice(i, 0, h("lisp-cursor"))
    }
    console.log(props.path, children)
    return h("lisp-buffer", {}, children)
  }
}

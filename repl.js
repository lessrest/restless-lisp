import Context from "./react-context.js"
import { Term } from "./term.js"
import { packages, read, stream } from "./lisp.js"
import { execute } from "./keval.js"

import { Component, h } from "./vendor/preact.js"

export class Repl extends Component {
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

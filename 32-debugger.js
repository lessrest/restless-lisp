import { keval }
  from "./02-keval.js"
import { html }
  from "./11-html.js"
import Context
  from "./12-react-context.js"
import { Term }
  from "./30-term.js"

import { Component, h }
  from "./vendor/preact.js"

export class Debugger extends Component {
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

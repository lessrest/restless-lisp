import { example, eval_ } from "./lisp.js"
eval_({
  print: x => console.log(x),
}, example)

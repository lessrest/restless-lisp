import { example, EVAL } from "./lisp.js"
EVAL({ print: x => console.log(x) }, example)

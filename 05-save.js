export function save(x) {
  let map = new Map
  let array = []
  save_(x, map, array)
  return {
    root: save_(x, map, array),
    ptrs: array,
  }
}

let proto = Object.getPrototypeOf({})

export function save_(x, map, array) {
  function recur(xx) { return save_(xx, map, array) }

  if (Object.is(x, null))
    return null
  else if (Object.is(x, undefined))
    throw new Error("cannot save undefined")
  else if (typeof x === "string")
    return x
  else if (typeof x === "number")
    return x
  else {
    if (map.has(x))
      return { "%@": map.get(x) }
    else {
      let y, i = array.length

      map.set(x, array.length)
      array.push(undefined)

      if (Array.isArray(x)) {
        y = x.map(z => recur(z))
      }

      else if (typeof x === "object") {
        if (x instanceof Map) {
          y = {
            "%@Map": Array.from(x.entries()).map(
              ([k, v]) => [recur(k), recur(v)])
          }
        }

        else {
          if (Object.getPrototypeOf(x) !== proto) {
            throw new Error("something")
          }

          y = Object.fromEntries(
            Object.entries(x).map(
              ([k, v]) => [k, recur(v)]))
        }
      }

      else throw new Error("error")

      array[i] = y
      return y
    }
  }
}

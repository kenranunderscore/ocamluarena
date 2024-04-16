type t =
  { red : int
  ; green : int
  ; blue : int
  }

let make ~red ~green ~blue = { red; green; blue }
let random () = { red = Random.int 256; green = Random.int 256; blue = Random.int 256 }

open Ctypes

open Foreign

let detect = foreign "check_file" (string @-> returning int)

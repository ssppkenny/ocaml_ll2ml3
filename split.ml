open Ctypes

open Foreign

let mysplit = foreign "run_split" (string @-> returning int)



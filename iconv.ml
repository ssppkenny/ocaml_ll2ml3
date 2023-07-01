open Ctypes

open Foreign

let iconv = foreign "conv_to_utf8" (string @-> string @-> returning int)

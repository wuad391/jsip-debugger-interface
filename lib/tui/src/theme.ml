open! Core
module Attr = Bonsai_term.Attr

let color hex =
  Attr.Color.rgb
    ~r:((hex lsr 16) land 0xff)
    ~g:((hex lsr 8) land 0xff)
    ~b:(hex land 0xff)
;;

(* the mockup's warm-gray palette, straight from the design file *)
let bg = color 0xeae7e7
let panel_bg = color 0xf8f4f4
let strip_bg = color 0xf1eeee
let accent = color 0xb68235
let accent_bg = color 0xfff3e4
let accent_deep = color 0x7d5411
let fresh = color 0x7d5411
let text = color 0x201f1d
let secondary = color 0x605d5d
let muted = color 0x7d7979
let faint = color 0x9b9797
let ghost = color 0xbab6b6
let border = color 0xd7d3d3
let border_strong = color 0xbab6b6
let hairline = color 0xe2dede
let tick_past = color 0xd7bd94
let app_purple = color 0x8046a8
let keyword = color 0x8046a8
let ident = color 0x2c6284
let string_lit = color 0x3d6b46
let number = color 0xa06f24
let fg c = Attr.fg c
let fg' c = [ Attr.fg c ]

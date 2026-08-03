open! Core
module Attr = Bonsai_term.Attr

let color hex =
  Attr.Color.rgb
    ~r:((hex lsr 16) land 0xff)
    ~g:((hex lsr 8) land 0xff)
    ~b:(hex land 0xff)
;;

(* the design mockup's warm-gray palette, re-pitched for a dark terminal: the
   same hue relationships (warm grays, gold accent, purple/blue/green syntax)
   with lightness inverted — backgrounds sink to near-black warm grays, text
   rises to warm off-white, and the accent gold brightens so it still carries
   the highlights *)
let bg = color 0x151312
let panel_bg = color 0x1d1b19
let accent = color 0xd4a24e
let fresh = color 0x8fd694

(* selection and position live in one hue so the eye can chase it across
   panes: a bright blue for bars, washes, and the current tick *)
let highlight = color 0x4da3ff
let highlight_bg = color 0x16324c
let highlight_deep = color 0xa6d2ff

(* heap node cards outline in a calmer blue than the selection *)
let card_border = color 0x5d8cc2
let text = color 0xe6e1dc
let secondary = color 0xb8b2ac
let muted = color 0x948e88
let faint = color 0x7a7570
let ghost = color 0x57534e
let border = color 0x4b4741
let border_strong = color 0x5f5a53
let hairline = color 0x2a2825
let tick_past = color 0x2e5578
let app_purple = color 0xc39ae2
let keyword = color 0xc39ae2
let ident = color 0x7fb8dc
let string_lit = color 0x96c7a2
let number = color 0xd8a45e
let fg c = Attr.fg c
let fg' c = [ Attr.fg c ]

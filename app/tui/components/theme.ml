open! Core
module Attr = Bonsai_term.Attr

let color hex =
  Attr.Color.rgb
    ~r:((hex lsr 16) land 0xff)
    ~g:((hex lsr 8) land 0xff)
    ~b:(hex land 0xff)
;;

(* the design mockup's warm-gray palette, re-pitched for a dark terminal —
   the grays run a shade brighter than a straight inversion so they stay
   legible on a large dark surface: the same hue relationships (warm grays,
   gold accent, purple/blue/green syntax) with lightness inverted —
   backgrounds sink to near-black warm grays, text rises to warm off-white,
   and the accent gold brightens so it still carries the highlights *)
(* one surface for the whole screen: panes are not boxes and do not sit on
   their own slab, so a second background would only ever read as a stray
   band where two panes meet *)
let bg = color 0x1d1b19
let accent = color 0xd4a24e
let fresh = color 0x8fd694

(* selection and position live in one hue so the eye can chase it across
   panes: a bright blue for bars, washes, and the current tick *)
let highlight = color 0x4da3ff
let highlight_bg = color 0x16324c
let highlight_deep = color 0xa6d2ff

(* the keyboard cursor rides alongside the selection, so it needs a hue the
   blue cannot be confused with at a glance — orange, its complement *)
let cursor = color 0xf2913d
let cursor_bg = color 0x4a2c10
let cursor_deep = color 0xffc489

(* heap node cards outline in a calmer blue than the selection *)
let card_border = color 0x5d8cc2

(* the rails between cards are the diagram's pointers, not chrome — brighter
   than the pane dividers so the edges read ahead of them *)
let rail = color 0x9c958c
let text = color 0xefebe6
let secondary = color 0xd8d3cd
let muted = color 0xbbb5ad
let faint = color 0xa39d95
let ghost = color 0x847e76
let border = color 0x6f6a62
let hairline = color 0x4a4640
let tick_past = color 0x2e5578
let app_purple = color 0xc39ae2

(* the app's purple, in its syntax-highlighting role *)
let keyword = app_purple
let ident = color 0x7fb8dc
let string_lit = color 0x96c7a2
let number = color 0xd8a45e

(* the compute-heat ramp, cold to hot: it starts in a cool slate that cannot
   be confused with the text grays, warms through the accent gold's register,
   and ends in a red deeper than [cursor]'s orange so the hottest cell and
   the keyboard cursor never read as one *)
let heat_ramp =
  [| color 0x5a6a78
   ; color 0x8a7a58
   ; color 0xc09149
   ; color 0xdf7038
   ; color 0xe05545
  |]
;;

(* compute shares are heavy-tailed — one hot function next to many tepid ones
   — so the buckets are log-spaced, not linear *)
let heat_thresholds = [ 0.20, 4; 0.08, 3; 0.03, 2; 0.01, 1 ]

let heat ~share =
  List.find_map heat_thresholds ~f:(fun (threshold, index) ->
    match Float.( >= ) share threshold with
    | true -> Some heat_ramp.(index)
    | false -> None)
  |> Option.value ~default:heat_ramp.(0)
;;

let fg c = Attr.fg c
let fg' c = [ Attr.fg c ]

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

(* the one exception, and the reason it is one: a pop-out is meant to be read
   as sitting ABOVE the panes rather than among them, so it gets the shade
   that says so. A border alone reads as a pane with an odd frame. *)
let raised = color 0x272320

(* the call stack's alternating rows: a clearly lighter warm gray — it has to
   read as a band at a glance, not on inspection, while staying far enough
   under the text and the picked rows' washes *)
let stripe_bg = color 0x312c26
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

(* One node can be drawn twice — its own row, and a pointer at it from a
   structure that shares it — and both light up so the pair can be found.
   These are the muted halves of the two washes above: dim enough that the
   row actually being pointed at still wins, strong enough to spot from
   across the pane, which a tinted word alone was not. *)
let cursor_echo = color 0x3b2712
let highlight_echo = color 0x122536
let text = color 0xefebe6
let secondary = color 0xd8d3cd
let muted = color 0xbbb5ad
let faint = color 0xa39d95
let ghost = color 0x847e76
let border = color 0x6f6a62
let hairline = color 0x4a4640
let tick_past = color 0x2e5578
let app_purple = color 0xc39ae2

(* the timeline's activity shading: a cell containing allocation bursts
   brightens within its own hue — past stays in the position blue's register,
   future in the idle gray warming toward the accent — so density reads as
   intensity while past/current/future keep their meaning. Index by rising
   density, [0] = quiet. *)
let tick_past_ramp = [| tick_past; color 0x3f78ab; color 0x5b9bd6 |]
let tick_future_ramp = [| hairline; color 0x6b6255; color 0x93825f |]

(* most steps allocate a little and a few allocate a lot; the buckets are
   spaced so only genuine bursts reach the bright stop *)
let tick_density ramp ~density =
  match Float.( >= ) density 0.5, Float.( >= ) density 0.15 with
  | true, _ -> ramp.(2)
  | false, true -> ramp.(1)
  | false, false -> ramp.(0)
;;

(* the diagram pop-out's two strokes: a node's box, and the rails between
   boxes. The rails are the diagram's edges — the actual pointers — so they
   read a shade ahead of the boxes that hang off them. *)
let card_border = color 0x5d8cc2
let rail = color 0x9c958c

(* The heap's two text registers, re-pitched at Robyn's request (2026-08-05):
   the pane read as busy with values in blue, so the pair swapped. Types are
   the calm blue now — the same register as the source pane's identifiers —
   and values the warm one. The value orange sits well under [cursor]'s
   bright orange and to the red side of [accent]'s gold, so neither the
   aiming color nor the brand highlight has a twin. Pointedly not green,
   which the [new] tag owns. *)
let type_name = color 0x7fb8dc
let value_text = color 0xd08a54

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

module For_testing = struct
  (* every role by name, so a test can say which gray a cell is wearing
     rather than matching hex — and so the answer lives beside the
     definitions rather than in a copy that goes stale *)
  let roles =
    [ "bg", bg
    ; "raised", raised
    ; "stripe_bg", stripe_bg
    ; "accent", accent
    ; "fresh", fresh
    ; "highlight", highlight
    ; "highlight_bg", highlight_bg
    ; "highlight_deep", highlight_deep
    ; "cursor", cursor
    ; "cursor_bg", cursor_bg
    ; "cursor_deep", cursor_deep
    ; "cursor_echo", cursor_echo
    ; "highlight_echo", highlight_echo
    ; "card_border", card_border
    ; "rail", rail
    ; "type_name", type_name
    ; "value_text", value_text
    ; "text", text
    ; "secondary", secondary
    ; "muted", muted
    ; "faint", faint
    ; "ghost", ghost
    ; "border", border
    ; "hairline", hairline
    ; "tick_past", tick_past
    ; "app_purple", app_purple
    ; "ident", ident
    ; "string_lit", string_lit
    ; "number", number
    ]
    (* the timeline ramps' own stops, by index; stop 0 of each is the flat
       color named above, which wins by listing first *)
    @ List.concat_map
        [ "tick_past_ramp", tick_past_ramp
        ; "tick_future_ramp", tick_future_ramp
        ]
        ~f:(fun (name, ramp) ->
          List.mapi (Array.to_list ramp) ~f:(fun index color ->
            [%string "%{name}%{index#Int}"], color))
  ;;

  let color_name color =
    List.find_map roles ~f:(fun (name, role) ->
      match Attr.Color.equal color role with
      | true -> Some name
      | false -> None)
    |> Option.value ~default:(Sexp.to_string [%sexp (color : Attr.Color.t)])
  ;;
end

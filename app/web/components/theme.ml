open! Core
module Syntax = Jsip_parsing.Syntax

module Color = struct
  type t = string [@@deriving equal]
end

type t =
  { name : string
  ; bg : Color.t
  ; strip_bg : Color.t
  ; border : Color.t
  ; panel_border : Color.t
  ; text : Color.t
  ; bright : Color.t
  ; heading : Color.t
  ; dim : Color.t
  ; faint : Color.t
  ; ghost : Color.t
  ; separator : Color.t
  ; hairline : Color.t
  ; edge : Color.t
  ; edge_label : Color.t
  ; accent : Color.t
  ; accent_bright : Color.t
  ; gold : Color.t
  ; fresh : Color.t
  ; selection_bg : Color.t
  ; selection_border : Color.t
  ; selection_text : Color.t
  ; range_bg : Color.t
  ; range_border : Color.t
  ; stripe_bg : Color.t
  ; tooltip_bg : Color.t
  ; tooltip_border : Color.t
  ; hud_bg : Color.t
  ; overlay_bg : Color.t
  ; minimap_bg : Color.t
  ; syntax_plain : Color.t
  ; syntax_comment : Color.t
  ; syntax_string : Color.t
  ; syntax_keyword : Color.t
  ; syntax_uident : Color.t
  ; syntax_number : Color.t
  ; syntax_operator : Color.t
  ; syntax_label : Color.t
  ; call_name : Color.t
  ; heat_stops : (float * Color.t) list
  ; box_block : Color.t * Color.t * Color.t
  ; box_nil : Color.t * Color.t * Color.t
  ; box_shared : Color.t * Color.t * Color.t
  ; node_key : Color.t
  ; node_reference : Color.t
  ; node_value : Color.t
  ; raw_key : Color.t
  ; raw_value : Color.t
  ; minimap_block : Color.t
  ; minimap_shared : Color.t
  ; minimap_nil : Color.t
  }
[@@deriving equal]

(* the imported design's cool-dark palette, verbatim *)
let dark =
  { name = "dark"
  ; bg = "#101216"
  ; strip_bg = "#15171c"
  ; border = "#262b34"
  ; panel_border = "#2b303a"
  ; text = "#c3c9d4"
  ; bright = "#e2e6ec"
  ; heading = "#aeb6c2"
  ; dim = "#7f8899"
  ; faint = "#6a7382"
  ; ghost = "#5d6675"
  ; separator = "#39404c"
  ; hairline = "#252b34"
  ; edge = "#333b46"
  ; edge_label = "#7d8899"
  ; accent = "#c8763a"
  ; accent_bright = "#e08a4a"
  ; gold = "#d9a05b"
  ; fresh = "#8fd694"
  ; selection_bg = "#17509c"
  ; selection_border = "#8ab6ea"
  ; selection_text = "#ffffff"
  ; range_bg = "#1c3f7a"
  ; range_border =
      "#6f9bd8"
      (* the call stack's every-other-row band. The mockup's was a whisper; a
         stack is read by scanning across a row, so the banding has to
         survive being looked past. *)
  ; stripe_bg = "#1e242f"
  ; tooltip_bg = "rgba(13,15,19,.97)"
  ; tooltip_border = "#3c4350"
  ; hud_bg = "rgba(16,18,22,.86)"
  ; overlay_bg = "rgba(16,18,22,.94)"
  ; minimap_bg = "rgba(16,18,22,.9)"
  ; syntax_plain = "#b8bec9"
  ; syntax_comment = "#6b7382"
  ; syntax_string = "#cdd3dc"
  ; syntax_keyword = "#a385c9"
  ; syntax_uident = "#5f9fd6"
  ; syntax_number = "#d0a05b"
  ; syntax_operator = "#7a8290"
  ; syntax_label = "#93a0b0"
  ; call_name = "#d4794f"
  ; heat_stops =
      [ 0.0, "#1b212a"
      ; 0.35, "#2f6fb8"
      ; 0.62, "#7d8a5a"
      ; 0.82, "#c9a24a"
      ; 1.0, "#d4794f"
      ]
  ; box_block = "#171c23", "#4a6076", "#cdd3dc"
  ; box_nil = "#14161a", "#3a414c", "#5d6675"
  ; box_shared = "#191d24", "#556579", "#93a0b0"
  ; node_key = "#7d8899"
  ; node_reference = "#6f8fae"
  ; node_value = "#d0a05b"
  ; raw_key = "#5f6a7a"
  ; raw_value = "#8ea0b4"
  ; minimap_block = "#39485a"
  ; minimap_shared = "#6d5433"
  ; minimap_nil = "#2b3038"
  }
;;

(* the same hues with lightness inverted: paper surfaces, ink text, the
   accent orange darkened just enough to keep its contrast *)
let light =
  { name = "light"
  ; bg = "#f5f6f8"
  ; strip_bg = "#eceef2"
  ; border = "#d7dbe2"
  ; panel_border = "#c9cfd8"
  ; text = "#333a46"
  ; bright = "#1c2129"
  ; heading = "#4c5665"
  ; dim = "#6b7482"
  ; faint = "#8a93a1"
  ; ghost = "#a5adba"
  ; separator = "#c5cbd4"
  ; hairline = "#e4e7ec"
  ; edge = "#b9c1cc"
  ; edge_label = "#7a8494"
  ; accent = "#b95f24"
  ; accent_bright = "#a04e15"
  ; gold = "#a97c26"
  ; fresh = "#2e8b57"
  ; selection_bg = "#17509c"
  ; selection_border = "#8ab6ea"
  ; selection_text = "#ffffff"
  ; range_bg = "#d7e6fb"
  ; range_border = "#4a7fc1"
  ; stripe_bg = "#e2e7ef"
  ; tooltip_bg = "rgba(252,253,255,.97)"
  ; tooltip_border = "#b9c1cc"
  ; hud_bg = "rgba(255,255,255,.86)"
  ; overlay_bg = "rgba(255,255,255,.94)"
  ; minimap_bg = "rgba(255,255,255,.9)"
  ; syntax_plain = "#3a414d"
  ; syntax_comment = "#8a93a1"
  ; syntax_string = "#54705e"
  ; syntax_keyword = "#7b4fb0"
  ; syntax_uident = "#2f6fad"
  ; syntax_number = "#a06a1e"
  ; syntax_operator = "#707a88"
  ; syntax_label = "#5d6a7c"
  ; call_name = "#b05426"
  ; heat_stops =
      [ 0.0, "#e3e7ee"
      ; 0.35, "#5b8fd0"
      ; 0.62, "#8f9b66"
      ; 0.82, "#c9a24a"
      ; 1.0, "#c9703f"
      ]
  ; box_block = "#ffffff", "#7d97b3", "#2d3743"
  ; box_nil = "#f2f4f7", "#c3cbd6", "#a5adba"
  ; box_shared = "#f6f8fb", "#8fa3bb", "#5d6a7c"
  ; node_key = "#7a8494"
  ; node_reference = "#3e648d"
  ; node_value = "#a05c22"
  ; raw_key = "#8a93a1"
  ; raw_value = "#4c657e"
  ; minimap_block = "#b3c2d4"
  ; minimap_shared = "#c9a273"
  ; minimap_nil = "#dde2ea"
  }
;;

let of_token t (token : Syntax.Token.t) =
  match token with
  | Keyword -> t.syntax_keyword
  | Uident -> t.syntax_uident
  | String -> t.syntax_string
  | Number -> t.syntax_number
  | Comment -> t.syntax_comment
  | Operator -> t.syntax_operator
  | Plain -> t.syntax_plain
;;

let parse_hex hex =
  let channel at = Int.of_string ("0x" ^ String.sub hex ~pos:at ~len:2) in
  channel 1, channel 3, channel 5
;;

let mix a b ~amount =
  let ra, ga, ba = parse_hex a in
  let rb, gb, bb = parse_hex b in
  let blend x y =
    Int.of_float
      (Float.round_nearest
         (Float.of_int x +. ((Float.of_int y -. Float.of_int x) *. amount)))
  in
  sprintf "#%02x%02x%02x" (blend ra rb) (blend ga gb) (blend ba bb)
;;

let heat_color t value =
  let value = Float.clamp_exn value ~min:0. ~max:1. in
  let rec walk stops =
    match stops with
    | [] | [ ((_ : float), (_ : Color.t)) ] -> t.call_name
    | (previous_stop, previous) :: ((stop, color) :: _ as rest) ->
      (match Float.( <= ) value stop with
       | false -> walk rest
       | true ->
         let amount = (value -. previous_stop) /. (stop -. previous_stop) in
         mix previous color ~amount)
  in
  walk t.heat_stops
;;

(* the call stack's compute ramp — the TUI's warm five stops, so a function
   reads at the same intensity in both interfaces; shares are heavy-tailed,
   so the buckets are log-spaced exactly as the TUI spaces them *)
let stack_heat_ramp =
  [| "#5a6a78"; "#8a7a58"; "#c09149"; "#df7038"; "#e05545" |]
;;

let heat_thresholds = [ 0.20, 4; 0.08, 3; 0.03, 2; 0.01, 1 ]

let ramp_index ~share =
  List.find_map heat_thresholds ~f:(fun (threshold, index) ->
    match Float.( >= ) share threshold with
    | true -> Some index
    | false -> None)
  |> Option.value ~default:0
;;

let stack_heat ~share = stack_heat_ramp.(ramp_index ~share)

(* the flame drawer's ramp, in blue like the TUI's: boxes are filled, so the
   stops stay light enough for dark label ink, and the neutral is off the
   ramp so "no data" cannot be misread as "cold" *)
let flame_ramp = [| "#7d8f9c"; "#5f9fc4"; "#3fb0e0"; "#2ec5f5"; "#5fe0ff" |]
let flame_neutral = "#454b50"
let flame_label = "#0f1416"
let flame_label_neutral = "#b6bcc0"
let flame ~share = flame_ramp.(ramp_index ~share)
let fade t fill = mix fill t.bg ~amount:0.62

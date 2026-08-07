(** The web interface's palette, as CSS color strings — one record per mode,
    so the light/dark toggle is a change of value, not a change of code path.

    {!dark} is the imported design's cool-dark scheme verbatim; {!light} is
    the same hues with lightness inverted. Every pane and the canvas draw
    from the record they are handed, so the whole screen flips as one surface
    — don't inline raw colors in views. The compute and flame ramps are
    shared between modes (they are the TUI's, so a function reads at the same
    intensity in both interfaces), as is the flame drawer's filled-box ink. *)

open! Core

module Color : sig
  type t = string [@@deriving equal]
end

type t =
  { name : string (** ["dark"] / ["light"] — the toggle chip's label *)
  ; bg : Color.t
  ; strip_bg : Color.t
  ; border : Color.t
  ; panel_border : Color.t
  ; text : Color.t
  ; bright : Color.t
  ; heading : Color.t
  ; dim : Color.t (** the text grays, lightest role to darkest *)
  ; faint : Color.t
  ; ghost : Color.t
  ; separator : Color.t
  ; hairline : Color.t
  ; edge : Color.t
  (** pointer strokes on the heap canvas — WHITE against the dark theme, ink
      against the light one, rather than a panel gray: zoomed out these lines
      and the box borders are all there is to see *)
  ; edge_label : Color.t
  ; accent : Color.t
  (** the heap's burnt orange: its pane border, fold markers, the filter
      prompt, the HUD's model word *)
  ; accent_bright : Color.t
  ; gold : Color.t (** transient state — playing, pulses, minimap frame *)
  ; fresh : Color.t (** the "allocated at this step" green *)
  ; progress : Color.t
  (** the timeline's played span: one flat blue, so how far in you are is a
      length rather than a shade *)
  ; selection_bg : Color.t
  (** the blue wash that follows the current step and frame *)
  ; selection_border : Color.t
  ; selection_text : Color.t
  ; range_bg : Color.t (** the active line's [char_range] marking *)
  ; range_border : Color.t
  ; stripe_bg : Color.t (** the call stack's alternating rows *)
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
  ; call_name : Color.t (** the warm orange a callee's name renders in *)
  ; heat_stops : (float * Color.t) list
  (** the timeline's activity gradient, quiet to hot *)
  ; box_block : Color.t * Color.t * Color.t
  (** a heap box's [(fill, border, ink)] *)
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

val dark : t
val light : t
val of_token : t -> Jsip_parsing.Syntax.Token.t -> Color.t

(** Linear blend of two [#rrggbb] colors; [amount] 0 keeps the first. *)
val mix : Color.t -> Color.t -> amount:float -> Color.t

(** {!heat_stops} sampled continuously. *)
val heat_color : t -> float -> Color.t

(** The call stack's compute ramp and its log-spaced buckets — the TUI's. *)
val stack_heat_ramp : Color.t array

val stack_heat : share:float -> Color.t

(** The flame drawer's filled-box ramp, its off-ramp neutral for "no data",
    and the label inks that sit on both. *)
val flame_ramp : Color.t array

val flame_neutral : Color.t
val flame_label : Color.t
val flame_label_neutral : Color.t
val flame : share:float -> Color.t

(** A faded structure's stroke: the color sunk most of the way into [t]'s
    surface. *)
val fade : t -> Color.t -> Color.t

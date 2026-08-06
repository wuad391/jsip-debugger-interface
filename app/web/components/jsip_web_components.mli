(** The web interface's pure building blocks.

    Everything a browser is NOT needed for: the heap pane's two readings —
    the canvas scene ({!Heap_scene}) with its geometry ({!Heap_layout}), and
    the outline ({!Heap_outline}) its other tab shows — plus the stack,
    source, timeline and flame panes as data ({!Stack_rows}, {!Source_model},
    {!Timeline_model}, {!Flame_math}), themed by {!Theme}. [Jsip_web] renders
    these with Bonsai and a canvas widget; the tests read them directly, over
    the vendored golden dumps. *)

module Flame_math = Flame_math
module Heap_layout = Heap_layout
module Heap_outline = Heap_outline
module Heap_scene = Heap_scene
module Source_model = Source_model
module Stack_rows = Stack_rows
module Theme = Theme
module Timeline_model = Timeline_model

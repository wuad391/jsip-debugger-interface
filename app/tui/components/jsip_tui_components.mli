(** The interface's reusable [Bonsai_term] building blocks.

    Pure view builders and their shared vocabulary, with no app state of
    their own: the panes ({!Stack_pane}, {!Source_pane}, {!Heap_pane}) and
    the {!Flame_pane} drawer beneath the heap are functions over
    {!Jsip_replay.Replay} data, the bars ({!Transport}, {!Session_bar}) frame
    the screen's top and bottom, and everything is themed by {!Theme}, framed
    by {!Panel}, placed by {!Layout}, and wrapped by {!Wrap}. [Jsip_tui.App]
    wires them to the event loop. *)

module Flame_pane = Flame_pane
module Heap_pane = Heap_pane
module Layout = Layout
module Panel = Panel
module Source_pane = Source_pane
module Stack_pane = Stack_pane
module Syntax = Syntax
module Theme = Theme
module Session_bar = Session_bar
module Transport = Transport
module Wrap = Wrap

(** The debugger's terminal interface, built on [Bonsai_term].

    {!App} owns the state and the event loop; the pane modules are pure view
    builders over {!Jsip_replay.Replay} data, themed by {!Theme} and framed
    by {!Panel}. {!Layout} decides where panes sit so drawing and mouse
    hit-testing agree. *)

module App = App
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

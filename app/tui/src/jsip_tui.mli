(** The debugger's terminal interface, built on [Bonsai_term].

    {!App} owns the state and the event loop; the reusable building blocks —
    the panes, the bars, {!Theme}, {!Panel}, {!Layout} — live in
    [Jsip_tui_components] (../components) and are re-exported here so
    consumers keep one entry point. *)

module App = App
module Heap_pane = Jsip_tui_components.Heap_pane
module Layout = Jsip_tui_components.Layout
module Panel = Jsip_tui_components.Panel
module Source_pane = Jsip_tui_components.Source_pane
module Stack_pane = Jsip_tui_components.Stack_pane
module Syntax = Jsip_tui_components.Syntax
module Theme = Jsip_tui_components.Theme
module Session_bar = Jsip_tui_components.Session_bar
module Transport = Jsip_tui_components.Transport
module Wrap = Jsip_tui_components.Wrap

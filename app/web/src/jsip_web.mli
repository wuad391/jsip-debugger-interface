(** The web interface: Bonsai views over {!Jsip_web_components}'s data, plus
    the canvas heap widget. [app/web/client] is the js_of_ocaml entry point
    that fetches the inputs and starts {!App}. *)

module Action = Action
module App = App
module Flame_view = Flame_view
module Heap_widget = Heap_widget
module Session_view = Session_view
module Source_view = Source_view
module Stack_view = Stack_view
module Styles = Styles
module Timeline_view = Timeline_view

(** Core types shared across the JSIP Debugger Interface.

    Foundational data — functions calls, arguments etc. — used by every other
    layer. *)

module Function_info = Function_info
module Snapshot = Snapshot
module Argument = Argument
module Location = Location
module Registry_entry = Registry_entry
module Registry_delta = Registry_delta
module Scope = Scope
module Type_info = Type_info
module Source_file = Source_file
module Call = Call
module Call_stack = Call_stack
module Heat_profile = Heat_profile
module Flame_tree = Flame_tree

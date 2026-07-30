open! Core
open Jsip_types

module Step = struct
  type t =
    { call : Call.t
    ; frames : Call.t list
    ; new_addresses : Snapshot.Address.Set.t
    ; description : string
    }
end

type t =
  { steps : Step.t Array.t
  ; files : string list
  }

let addresses_of_snapshot (snapshot : Snapshot.t) =
  let rec walk (node : Snapshot.Node.t) acc =
    let acc = Set.add acc node.virtual_address in
    let acc =
      List.fold node.block ~init:acc ~f:(fun acc (_label, block) ->
        match block with
        | Address address -> Set.add acc address
        | Int _ | Float _ | String _ | Int32 _ | Int64 _ | Nativeint _
        | Float_array _ | Id _ ->
          acc)
    in
    List.fold node.children ~init:acc ~f:(fun acc child -> walk child acc)
  in
  walk snapshot.root_node Snapshot.Address.Set.empty
;;

let description (call : Call.t) =
  let fn = Function_info.display call.info.function_info in
  let args =
    List.map call.info.arguments ~f:Argument.display
    |> String.concat ~sep:" "
  in
  let callee =
    match args with "" -> fn | args -> [%string "%{fn} %{args}"]
  in
  [%string "%{callee} \u{2014} %{Location.display call.info.location}"]
;;

let create call_stack =
  let seen = ref Snapshot.Address.Set.empty in
  let steps =
    Array.init (Call_stack.length call_stack) ~f:(fun step ->
      let call = Call_stack.call_exn call_stack ~step in
      let addresses = addresses_of_snapshot call.info.snapshot in
      let new_addresses = Set.diff addresses !seen in
      seen := Set.union !seen addresses;
      { Step.call
      ; frames = Call_stack.frames_at call_stack ~step
      ; new_addresses
      ; description = description call
      })
  in
  let files =
    Array.fold steps ~init:[] ~f:(fun acc (step : Step.t) ->
      let file = Location.file_path step.call.info.location in
      match List.mem acc file ~equal:String.equal with
      | true -> acc
      | false -> file :: acc)
    |> List.rev
  in
  { steps; files }
;;

let length t = Array.length t.steps
let step_exn t ~step = t.steps.(step)
let files t = t.files

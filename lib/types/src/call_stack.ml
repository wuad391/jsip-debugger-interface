open! Core

type t = { call_order : Call.t Array.t }

(* helper: pops the rest of the stack until top is less than incomming *)
let pop_viable_ranges
  depth_stack
  (output_array : (int * int) Array.t)
  incoming_index
  incoming_depth
  =
  let rec iterate_stack () =
    match Stack.top depth_stack with
    | Some (top_index, top_depth) ->
      (match top_depth <= incoming_depth with
       | false ->
         output_array.(top_index - 1) <- top_index, top_index;
         iterate_stack ()
       | true ->
         (match top_depth = incoming_depth with
          | true ->
            output_array.(top_index - 1) <- top_index, incoming_index - 1
          | false -> ()))
    | None -> failwith "CALL_STACK.create: failed branch setup in create"
  in
  iterate_stack ()
;;

let create ~parsed_info =
  (* iterate and get call_range *)
  let call_ranges = Array.create ~len:(Queue.length parsed_info) (0, 0) in
  let depth_stack = Stack.create () in
  Queue.iteri parsed_info ~f:(fun next_index next_info ->
    let next_depth = Call.Info.(next_info.depth) in
    let next_component = next_index, next_depth in
    (match Stack.is_empty depth_stack with
     | true -> ()
     | false ->
       let _top_index, top_depth = Stack.pop_exn depth_stack in
       (match top_depth < next_depth with
        | true -> ()
        | false ->
          pop_viable_ranges depth_stack call_ranges next_index next_depth));
    Stack.push depth_stack next_component);
  (* create call_order *)
  let calls = Array.create ~len:(Queue.length parsed_info) Call.empty in
  Queue.iteri parsed_info ~f:(fun index info ->
    let call = ({ info; range = call_ranges.(index - 1) } : Call.t) in
    calls.(index - 1) <- call);
  { call_order = calls }
;;

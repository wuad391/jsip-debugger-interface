open! Core

type t = { call_order : Call.t Array.t }

let create ~parsed_info =
  let length = Queue.length parsed_info in
  let ranges = Array.create ~len:length (0, 0) in
  (* Frames still on the stack, as [(index, depth)]. An event at depth [d]
     closes every open frame at depth >= [d]: a deeper frame's callee chain
     is over, and a same-depth frame is replaced by its sibling. A closed
     frame's range ends just before the closing event. *)
  let open_frames = Stack.create () in
  Queue.iteri parsed_info ~f:(fun index (info : Call.Info.t) ->
    let rec close_frames () =
      match Stack.top open_frames with
      | Some (open_index, open_depth) when open_depth >= info.depth ->
        ignore (Stack.pop_exn open_frames : int * int);
        ranges.(open_index) <- open_index, index - 1;
        close_frames ()
      | Some _ | None -> ()
    in
    close_frames ();
    Stack.push open_frames (index, info.depth));
  (* whatever is still open lives until the end of the dump *)
  Stack.iter open_frames ~f:(fun (open_index, _depth) ->
    ranges.(open_index) <- open_index, length - 1);
  let call_order =
    Array.init length ~f:(fun index ->
      Call.create ~info:(Queue.get parsed_info index) ~range:ranges.(index))
  in
  { call_order }
;;

let length t = Array.length t.call_order
let call_exn t ~step = t.call_order.(step)

let frames_at t ~step =
  Array.to_list t.call_order
  |> List.filter ~f:(fun (call : Call.t) ->
    let lo, hi = call.range in
    lo <= step && step <= hi)
;;

open! Core

type t =
  { call_order : Call.t Array.t
  ; live : int list Array.t
  }

let create ~parsed_info =
  let length = Queue.length parsed_info in
  let ranges = Array.create ~len:length (0, 0) in
  let live = Array.create ~len:length [] in
  (* The wire writes an event when its call COMPLETES, so a call's children
     lie immediately BEFORE it in the dump: its subtree is the maximal run of
     deeper events ending at its own. [pending] holds subtrees whose parent
     has not arrived yet, as [(first_index, depth)]; an event adopts every
     pending subtree strictly deeper than itself. A same-depth entry stays
     put — that is a completed sibling, waiting for the parent both of them
     share. *)
  let pending = Stack.create () in
  Queue.iteri parsed_info ~f:(fun index (info : Call.Info.t) ->
    let first = ref index in
    let rec adopt () =
      match Stack.top pending with
      | Some (child_first, child_depth) when child_depth > info.depth ->
        ignore (Stack.pop_exn pending : int * int);
        first := Int.min !first child_first;
        adopt ()
      | Some _ | None -> ()
    in
    adopt ();
    ranges.(index) <- !first, index;
    Stack.push pending (!first, info.depth));
  (* The frames live at an event are its ancestors — which, completing after
     it, all lie ahead: walking backward, the ancestor chain is the
     strictly-shallower suffix of what has been seen. Recorded outermost
     first, the event itself (the innermost frame) last. *)
  let ancestors = Stack.create () in
  for index = length - 1 downto 0 do
    let depth = (Queue.get parsed_info index).Call.Info.depth in
    let rec unwind () =
      match Stack.top ancestors with
      | Some ((_ : int), seen_depth) when seen_depth >= depth ->
        ignore (Stack.pop_exn ancestors : int * int);
        unwind ()
      | Some _ | None -> ()
    in
    unwind ();
    live.(index) <- List.rev_map (Stack.to_list ancestors) ~f:fst @ [ index ];
    Stack.push ancestors (index, depth)
  done;
  let call_order =
    Array.init length ~f:(fun index ->
      Call.create ~info:(Queue.get parsed_info index) ~range:ranges.(index))
  in
  { call_order; live }
;;

let length t = Array.length t.call_order
let call_exn t ~step = t.call_order.(step)

let frames_at t ~step =
  List.map t.live.(step) ~f:(fun index -> t.call_order.(index))
;;

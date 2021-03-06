open Base 
open Base.Ref 

type callback_id = int

type ('a, 'b) cell_kind =
    | Input of ('a ref)
    | Compute_1 of ('b ref) * ('a -> 'a)
    | Compute_2 of ('b ref) * ('b ref) * ('a -> 'a -> 'a)

type 'a cell = { 
    id: int;
    eq: ('a -> 'a -> bool) ref; 
    callbacks: (((callback_id ref) * (('a -> unit) ref)) list) ref;
    kind: ('a, 'a cell) cell_kind;
    refs: ((('a cell) ref) list) ref;
} 

let rec value_of { kind; _ } =
    match kind with 
    | Input v -> !v
    | Compute_1 (c, f) -> f (value_of !c)
    | Compute_2 (c1, c2, f) -> f (value_of !c1) (value_of !c2)

let create_id =
  let n = ref 0 in
  fun () ->
    let id = !n in
    Int.incr n;
    id

let create_input_cell ~value ~eq = {
    id = create_id ();
    eq = ref eq; 
    callbacks = ref []; 
    kind = Input (ref value);
    refs = ref [];
}

let create_compute_cell_1 c ~f ~eq = 
    let compute_cell = {
        id = create_id ();
        eq = ref eq; 
        callbacks = ref [];
        kind = (Compute_1 (ref c, f));
        refs = ref [];
    }
    in 
    c.refs := (ref compute_cell) :: !(c.refs);
    compute_cell

let create_compute_cell_2 c1 c2 ~f ~eq = 
    let compute_cell = {
        id = create_id ();
        eq = ref eq; 
        callbacks = ref [];
        kind = Compute_2 (ref c1, ref c2, f); 
        refs = ref [];
    }
    in 
    c1.refs := (ref compute_cell) :: !(c1.refs);
    c2.refs := (ref compute_cell) :: !(c2.refs);
    compute_cell

let add_callback { callbacks; _ } ~k =
    let id = create_id () in
    callbacks := (ref id, ref k) :: !callbacks;
    id

let remove_callback { callbacks; _ } id =
    callbacks := List.filter ~f:(fun (i, _) -> if !i <> id then true else false ) !callbacks;
    ()

let cell_val_eq cell v = 
    !(cell.eq) v (value_of cell)

let callbacks_do cell =
    let already_called = ref [] in
    let do_call id = 
        match List.find ~f:(fun t -> t = !id) !already_called with
        | None -> already_called := !id::!already_called; true 
        | _ -> false 
    in
    List.iter ~f:(fun (id, f) -> if do_call id then !f (value_of cell) else ()) !(cell.callbacks)

let iterate_cell_callbacks cells_with_vals = 
    let already_called = ref [] in 
    let called id = 
        match List.find ~f:(fun t -> t = id) !already_called with
        | None -> already_called := id::!already_called; false 
        | _ -> true 
    in
    List.iter ~f:(fun (old_val, cell) -> if (cell_val_eq cell old_val) || (called cell.id) then () else callbacks_do cell) cells_with_vals 

let ref_cells_with_vals cell_top = 
    let refs = ref [] in
    let rec loop cell_param = 
        match !(cell_param.refs) with
        | [] -> ()
        | cells_matched -> refs := List.append !refs cells_matched; List.iter ~f:(fun cell -> loop !cell) cells_matched
    in loop cell_top;
    List.map ~f:(fun c -> ((value_of !c), !c)) !refs

let set_value cell new_value =
    let cells_with_vals = 
        match cell.kind with 
        | Input v -> Some(v, (!v, cell)::(ref_cells_with_vals cell))
        | _ -> None
    in 
    match (cells_with_vals) with 
    | Some(v, cells) -> if !(cell.eq) !v new_value then () else v := new_value; iterate_cell_callbacks cells
    | None -> ()

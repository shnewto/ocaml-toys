open Base 
open Base.Ref 

(* module type MUTABLE_CALLBACK_MAP =
    sig
        type 'a mcallback_map

        val empty : unit -> 'a mcallback_map

        val add : callback_id -> 'a mstack -> 'a -> unit

        val get : 'a mstack -> 'a option
    end

module MutableCallbackMap : MUTABLE_CALLBACK_MAP =
    struct
        type 'a mcallback_map = (('a callback) Map.M(Int).t) ref

        let empty () : 'a mcallback_map = ref (Map.empty (module Base.Int))

        let add (m:'a mcallback_map) (k:callback_id) (d:'a callback) : unit =
            m := Map.add (!m) ~key:k ~data:d
        
        let remove (m:'a mcallback_map) (k:callback_id) : unit =
            m := Map.remove (!m) k
    end *)

type callback_id = int


type ('a, 'b) cell_kind =
    | Input of ('a ref)
    | Compute_1 of ('b ref) * ('a -> 'a)
    | Compute_2 of ('b ref) * ('b ref) * ('a -> 'a -> 'a)

type 'a cell = { 
    eq: ('a -> 'a -> bool) ref; 
    callbacks: ((int * ('a -> unit)) list) ref;
    incr_next: callback_id ref;
    kind: ('a, 'a cell) cell_kind;
} 

(* 
type 'a cell_kind =
    | Input of ('a ref)
    | Compute_1 of ('a ref) * ('a -> 'a)
    | Compute_2 of ('a ref) * ('a ref) * ('a -> 'a -> 'a)
 *)

let set_value { kind; callbacks; _ } new_value =
    match kind with 
    | Input v -> v := new_value; let _ = List.for_all ~f:(fun (_, f) -> f !(v); true ) !(callbacks) in ();
    | _ -> ()

let rec value_of { kind; _ } =
    match kind with 
    | Input v -> !v
    | Compute_1 (c, f) -> f (value_of !c)
    | Compute_2 (c1, c2, f) -> f (value_of !c1) (value_of !c2)

let create_input_cell ~value ~eq = {
    eq = ref eq; 
    callbacks = ref []; 
    incr_next = ref 0;
    kind = Input (ref value);
}

let create_compute_cell_1 c ~f ~eq = {
    eq = ref eq; 
    callbacks = ref [];
    incr_next = ref 0; 
    kind = (Compute_1 (ref c, f));
}

let create_compute_cell_2 c1 c2 ~f ~eq = {
    eq = ref eq; 
    callbacks = ref []; 
    incr_next = ref 0;
    kind = Compute_2 (ref c1, ref c2, f); 
}

let add_callback { incr_next; callbacks; _ } ~k =
    incr_next := !(incr_next) + 1;
    callbacks := (!(incr_next), k)::!(callbacks);
    !(incr_next)

let remove_callback { callbacks; _ } id =
    callbacks := List.filter ~f:(fun (i, _) -> if i <> id then true else false ) !(callbacks);
    

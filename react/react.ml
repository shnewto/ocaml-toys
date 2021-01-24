open Base 

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

type cell_kind =
    | Input
    | Compute_1
    | Compute_2

type 'a cell = { 
    value: 'a ref; 
    eq: 'a -> 'a -> bool; 
    callbacks: ((int * ('a -> unit)) list) ref;
    kind: cell_kind;
}

let make_callback_id = 
  let count = ref 0 in
  fun () ->
    Int.incr count;
    !count

let set_value_with_callback c v =
    c.value := ref v;
    match !(c.callbacks) with 
    | [] -> ()
    | xs -> List.for_all ~f:(fun (_, f) -> let () = f (ref v) in true ) xs

let set_value c v =
    match c.kind with 
    | Input -> set_value_with_callback c v; 
    | _ -> ()

let value_of { value } =
    !value

let create_input_cell ~value ~eq = {
    value = ref value; 
    eq; 
    callbacks = ref []; 
    kind = Input;
}

let create_compute_cell_1 c ~f ~eq = {
    value = ref (f (value_of c)); 
    eq; 
    callbacks = ref []; 
    kind = Compute_1;
}

let create_compute_cell_2 ca cb ~f ~eq = { 
    value = ref (f (value_of ca) (value_of cb)); 
    eq; 
    callbacks = ref []; 
    kind = Compute_2; 
}

let add_callback c ~k =
    c.callback := ref k;
    let callback_id = make_callback_id in 
        c.callback_id := ref callback_id;
        callback_id

let remove_callback c id =
    let callbacks = !(c.callbacks) in 
    c.callbacks := ref (List.filter ~f(fun (i, c) -> if i <> id then true else false ) callbacks);
    

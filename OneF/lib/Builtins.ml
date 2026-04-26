open Runtime.Runtime

module Builtins = struct
  let binop_base (op : value -> value -> value) : value =
    VBuiltin (fun a -> VBuiltin (fun b -> op a b))

  let int_binop_base op =
    let inner a b =
      match (a, b) with
      | VInt a, VInt b -> VInt (op a b)
      | _ -> failwith "Cannot apply int operator to non int"
    in
    binop_base inner

  let print =
    let inner = function
      | VString s ->
          let () = print_endline s in
          VUnit
      | _ -> failwith "Cannot print string"
    in
    VBuiltin inner

  let polycompare_base mapping =
    let inner a b =
      match (a, b) with
      | VInt a, VInt b -> VBool (compare a b |> mapping)
      | VFloat a, VFloat b -> VBool (compare a b |> mapping)
      | VString a, VString b -> VBool (compare a b |> mapping)
      | VUnit, VUnit -> VBool (compare () () |> mapping)
      | _ -> failwith "Cannot compare values"
    in
    binop_base inner

  let string_append =
    let inner a b =
      match (a, b) with
      | VString a, VString b -> VString (a ^ b)
      | _ -> failwith "String concatenation is for strings only"
    in
    binop_base inner

  let builtin_name_pairs =
    [
      ("+", int_binop_base ( + ));
      ("-", int_binop_base ( - ));
      ("*", int_binop_base ( * ));
      ("/", int_binop_base ( / ));
      ("^", string_append);
      ("<", polycompare_base (( = ) (-1)));
      (">", polycompare_base (( = ) 1));
      ("=", polycompare_base (( = ) 0));
      ("<>", polycompare_base (( <> ) 0));
      (">=", polycompare_base (( <> ) (-1)));
      ("<=", polycompare_base (( <> ) 1));
      ("напечатай", print);
    ]

  let builtins = builtin_name_pairs |> List.to_seq |> StringMap.of_seq
end

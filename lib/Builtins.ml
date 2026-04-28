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
          let () = CCUtf8_string.to_string s |> print_endline in
          VUnit
      | v ->
          failwith @@ "Cannot print non string: "
          ^ Runtime.Runtime.value_to_string v
    in
    VBuiltin inner

  let debug =
    let inner x =
      print_endline @@ Runtime.Runtime.value_to_string x;
      VUnit
    in
    VBuiltin inner

  let polycompare_base mapping =
    let inner a b =
      match (a, b) with
      | VInt a, VInt b -> VBool (compare a b |> mapping)
      | VFloat a, VFloat b -> VBool (compare a b |> mapping)
      | VString a, VString b -> VBool (compare a b |> mapping)
      | VUnit, VUnit -> VBool (compare () () |> mapping)
      | VChar a, VChar b -> VBool (Uchar.compare a b |> mapping)
      | _ -> failwith "Cannot compare values"
    in
    binop_base inner

  let string_append =
    let inner a b =
      match (a, b) with
      | VString a, VString b -> VString (CCUtf8_string.append a b)
      | _ -> failwith "String concatenation is for strings only"
    in
    binop_base inner

  let list_const =
    let inner a b =
      match (a, b) with
      | v, VList l -> VList (v :: l)
      | _ -> failwith "Cannot append to non list"
    in
    binop_base inner

  let explode_string =
    let inner x =
      match x with
      | VString x -> 
        let content = Containers.Utf8_string.to_list x |> List.map (fun v -> VChar v) in
        VList content
      | _ -> failwith "Can only explode string"
    in
    VBuiltin inner
  
  let implode_string =
    let get_uchars = function
      | VChar x -> x
      | _ -> failwith "Expected char"
    in
    let inner x =
      match x with
      | VList x -> 
        let str = List.map get_uchars x |> CCUtf8_string.of_list in
        VString str
      | _ -> failwith "Can only explode string"
    in
    VBuiltin inner

  let int_of_char =
    let inner a =
      match a with
      | VChar v -> VInt (Uchar.to_int v)
      | _ -> failwith "int_of_char: awaited char"
    in
    VBuiltin inner

  let char_of_int =
    let inner a =
      match a with
      | VInt i -> VChar (Uchar.of_int i)
      | _ -> failwith "char_of_int: awaited int"
    in
    VBuiltin inner

  let int_of_string =
    let inner a =
      match a with
      | VString v -> VInt (CCUtf8_string.to_string v |> int_of_string)
      | _ -> failwith "int_of_string: awaited string"
    in
    VBuiltin inner

  let string_of_int =
    let inner a =
      match a with
      | VInt i -> VString (string_of_int i |> CCUtf8_string.of_string_exn)
      | _ -> failwith "string_of_int: awaited int"
    in
    VBuiltin inner

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
      ("::", list_const);
      ("напечатай", print);
      ("дебаг", debug);
      ("список_из_строки", explode_string);
      ("строка_из_списка", implode_string);
      ("число_из_символа", int_of_char);
      ("символ_из_числа", char_of_int);
      ("число_из_строки", int_of_string);
      ("строка_из_числа", string_of_int);
    ]

  let builtins = builtin_name_pairs |> List.to_seq |> StringMap.of_seq
end

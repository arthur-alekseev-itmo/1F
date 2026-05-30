module SkibidIR = struct
  type location = Relocation of string | Exact of int
  [@@deriving show { with_path = false }]

  type skb_constant =
    | ScFloat of float
    | ScString of string
    | ScInt of int (* also bool *)
    | ScFnAddr of int
  [@@deriving show { with_path = false }]

  type skb_int_op = IAdd | IMul | ISub | IDiv | IMod
  [@@deriving show { with_path = false }]

  type skb_float_op = FAdd | FMul | FSub | FDiv
  [@@deriving show { with_path = false }]

  type skb_cmp_op = CEq | CNeq | CLt | CLe | CGt | CGe
  [@@deriving show { with_path = false }]

  (* logic operators do not exist! *)

  type skb_instr =
    | LoadConst of skb_constant
    | LoadLocal of int
    | StoreLocal of int
    | LoadGlobal of location
    | StoreGlobal of int
    | BranchTrue of location
    | BranchFalse of location
    | Branch of location
    | IntOperator of skb_int_op
    | FloatOperator of skb_float_op
    | CmpOperator of skb_cmp_op
    | FunctionAddress of location
    | Apply
    | ConstructTuple of int
    | DeconstructTuple of int
    | Drop
    | Sigbus
    | GetCtorTag
  [@@deriving show { with_path = false }]

  type skb_program = skb_instr array [@@deriving show { with_path = false }]

  let pp_skibidir (instructions : skb_program) =
    let rec inner index =
      if index >= Array.length instructions then []
      else
        let instr = Array.get instructions index in
        Fmt.str "SKB_%03d %s" index (show_skb_instr instr) :: inner (index + 1)
    in
    inner 0 |> String.concat "\n"


  let serialize_instr (instr : skb_instr) =
    let sized x = Bytes.create x in
    let with_pref p b =
      Bytes.set b 0 (Char.chr p);
      b
    in
    let with_int64 p b =
      Bytes.set_int64_ne b 1 (Int64.of_int p);
      b
    in
    let with_float64 p b =
      Bytes.set_int64_ne b 1 (Int64.bits_of_float p);
      b
    in
    let one_arg p i = sized 9 |> with_pref p |> with_int64 i in
    let zero_arg p = sized 1 |> with_pref p in
    let one_loc p i =
      match i with
      | Relocation _ -> failwith "Linking was not performed"
      | Exact i -> one_arg p i
    in
    match instr with
    | LoadConst (ScFnAddr i) | LoadConst (ScInt i) -> one_arg 0x1 i
    | LoadConst (ScString s) ->
        let string = CCUtf8_string.of_string_exn s in
        let string_len = CCUtf8_string.n_bytes string in
        let string_bytes = Bytes.of_string s in
        sized (9 + string_len) |> with_pref 0x2 |> with_int64 string_len
        |> fun b ->
        Bytes.blit b 9 string_bytes 0 string_len;
        b
    | LoadConst (ScFloat f) -> sized 9 |> with_pref 0x1 |> with_float64 f
    | LoadLocal i -> one_arg 0x3 i
    | StoreLocal i -> one_arg 0x4 i
    | LoadGlobal i -> one_loc 0x5 i
    | StoreGlobal i -> one_arg 0x6 i
    | BranchTrue i -> one_loc 0x7 i
    | BranchFalse i -> one_loc 0x8 i
    | Branch i -> one_loc 0x9 i
    | IntOperator _ -> failwith "TODO: Reserved skibid (RESV: 0xABCDE)i"
    | FloatOperator _ -> failwith "TODO: Reserved skibid (RESV: 0xF 11 12 13)"
    | CmpOperator CEq -> zero_arg 0x14
    | CmpOperator _ -> failwith "TODO: Reseved skibid (RESV: 0x15 16 17 18 19)"
    | FunctionAddress i -> one_loc 0x20 i
    | Apply -> zero_arg 0x21
    | ConstructTuple i -> one_arg 0x22 i
    | DeconstructTuple i -> one_arg 0x23 i
    | Drop -> zero_arg 0x24
    | Sigbus -> zero_arg 0xff
    | GetCtorTag -> zero_arg 0x25


  let serialize_skibidir (instructions : skb_program) =
    let empty = Bytes.create 0 in
    let oc = open_out_bin "out.1fbc" in
    Array.map serialize_instr instructions
    |> Array.to_list
    |> Bytes.concat empty
    |> output_bytes oc;
    close_out oc
end

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
    | BranchTrue of location * location
    | Branch of location
    | IntOperator of skb_int_op
    | FloatOperator of skb_int_op
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
end

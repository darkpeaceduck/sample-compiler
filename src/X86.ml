open Language.Value

type opnd = R of int | R8 of int | S of int | Farg of int | M of string | L of int | SC of string

let x86regs = [|
  "%eax";
  "%ebx";
  "%ecx";
  "%edx";
  "%esi";
  "%edi"
  |]

let x86regs8 = [|
  "%al";
  "%ah";
  "%bl";
  |]

let num_of_regs = Array.length x86regs
let word_size = 4

let eax = R 0
let ebx = R 1
let ecx = R 2
let edx = R 3
let esi = R 4
let edi = R 5

let unreserved_regs = [| esi; edi |]

let al = R8 0
let ah = R8 1
let bl = R8 2

type cmp_suf =
  | Eq | Neq | Less | Leq | G | Geq | Nz

type instr_suf =
  | Long | None

type special_suf = Prologue of int | Epilogue

type instr =
  | X86Add of opnd * opnd
  | X86Sub of opnd * opnd
  | X86Mul of opnd * opnd
  | X86Xor of opnd * opnd
  | X86Or of opnd * opnd
  | X86Div of opnd
  | X86Cmp of opnd * opnd
  | X86Cltd
  | X86Mov of opnd * opnd
  | X86Push of opnd
  | X86Pop of opnd
  | X86Ret
  | X86Call of string
  | X86Set of cmp_suf * opnd
  | X86And of instr_suf * opnd * opnd
  | AsmLabel of string
  | X86Jmp of string
  | X86Jz of string
  | Special of special_suf

type scope = GLOB | FUN

module SET = Set.Make (String)
module MAP = Map.Make (String)

class x86env =
object(self)
  
  val val_scope = ref GLOB;
  method set_scope scope_t = val_scope := scope_t
  method get_scope = !val_scope
  
  val local_vars = ref MAP.empty
  val allocated_local = ref 0;
  method assign_local x opnd = local_vars := MAP.add x opnd !local_vars ;
  method get_local x =
    try
      Some (MAP.find x !local_vars)
    with Not_found -> None
  
  method allocate_local =
    allocated_local := 1 + !allocated_local;
    if !allocated_local < Array.length unreserved_regs then
      Array.get unreserved_regs !allocated_local
    else
      S !allocated_local
  
  method create_local x =
    let result = self#allocate_local in
    self#assign_local x result;
    result
  
  method allocated_local = !allocated_local
  method release_locals =
    local_vars := MAP.empty;
    allocated_local := -1
  
  val global_vars = ref SET.empty
  method allocate_global x =
    global_vars := SET.add x !global_vars;
    M x
  
  method global_vars = SET.elements !global_vars
  
  method allocate x =
    if self#get_scope == FUN then
      self#create_local x
    else
      self#allocate_global x
  
  method get_opnd x =
    if self#get_scope == FUN then
      self#get_local x
    else
      Some (self#allocate_global x)
  
  method get_new_frame = self#release_locals
  
  method epilogue_label func_name = func_name^"_epilogue"
  
  method func_name name = "func"^name
  
  method builtin_func_name name = "bt_"^name
  
  method main_name = "main"
  
  val str_consts = ref []
  method get_str_form index = "str_"^string_of_int(index)
  method get_str_addr : string -> opnd =
    fun s ->
        str_consts := !str_consts @ [s];
        SC (self#get_str_form(List.length (!str_consts) - 1))
  method get_str_consts = !str_consts
  
end

module Show =
struct
  
  let opnd = function
    | R i -> x86regs.(i)
    | R8 i -> x86regs8.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | Farg i -> Printf.sprintf "%d(%%ebp)" ((i +1) * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i
    | SC i -> Printf.sprintf "$%s" i
  
  let cmp_suf = function
    | Eq -> "e"
    | Neq -> "ne"
    | Less -> "l"
    | Leq -> "le"
    | G -> "g"
    | Geq -> "ge"
    | Nz -> "nz"
  
  let instr_suf = function
    | Long -> "l"
    | None -> ""
  
  let special = function
    | Prologue allocated -> Printf.sprintf "\tpushl\t%%ebp\n\tmovl\t%%esp,\t%%ebp\n\tsubl\t$%d,\t%%esp" (allocated * word_size)
    | Epilogue -> "\tmovl\t%ebp,\t%esp\n\tpopl\t%ebp"
  
  let instr = function
    | X86Add (s1, s2) -> Printf.sprintf "\taddl\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Sub (s1, s2) -> Printf.sprintf "\tsubl\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Cmp (s1, s2) -> Printf.sprintf "\tcmp\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Xor (s1, s2) -> Printf.sprintf "\txorl\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Or (s1, s2) -> Printf.sprintf "\torl\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Mul (s1, s2) -> Printf.sprintf "\timull\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Div s -> Printf.sprintf "\tidivl\t%s" (opnd s)
    | X86Mov (s1, s2) -> Printf.sprintf "\tmovl\t%s,\t%s" (opnd s1) (opnd s2)
    | X86Push s -> Printf.sprintf "\tpushl\t%s" (opnd s )
    | X86Pop s -> Printf.sprintf "\tpopl\t%s" (opnd s )
    | X86Ret -> "\tret"
    | X86Call p -> Printf.sprintf "\tcall\t%s" p
    | X86Cltd -> "\tcltd"
    | X86Set (suf, p) -> Printf.sprintf "\tset%s\t%s" (cmp_suf suf) (opnd p)
    | X86And (suf, s1, s2) -> Printf.sprintf "\tand%s\t%s,\t%s" (instr_suf suf) (opnd s1) (opnd s2)
    | AsmLabel s -> Printf.sprintf "\t%s:" s
    | X86Jmp s -> Printf.sprintf "\tjmp %s" s
    | X86Jz s -> Printf.sprintf "\tjz %s" s
    | Special arg -> special arg
  
end

module Compile =
struct
  
  open StackMachine
  
  let stack_program env name code =
    let rec compile stack code =
      match code with
      | [] -> []
      | i:: code' ->
          let (stack', x86code) =
            let call (name, args_n) put_n =
              let ret_value_place = env#allocate_local in
              let rec unpack num_args left_stack =
                if (num_args == 0) then
                  ([], [X86Mov (eax, ret_value_place)], left_stack)
                else
                  match left_stack with
                  | x:: stack' ->
                      let (push_unp, pop_unp, st) = unpack (num_args - 1) stack' in
                      ([X86Push x] @ push_unp,
                        pop_unp @ [X86Pop eax], st)
                  | [] -> failwith "stack is empty, but some more args exits"
              in
              let pre, post, stack' = unpack args_n stack in
              let (put_n_args, pop_n_args) =
                if put_n then ([X86Push (L args_n)],[ X86Pop ebx])
                else ([], [])
              in
              (ret_value_place:: stack', pre @ put_n_args @ [X86Call (name)] @ pop_n_args @ post)
            in
            match i with
            | S_PUSH n' ->
                let res = match n' with
                  | Int n -> (L n:: stack, [])
                  | a -> (env#get_str_addr (to_string a) :: stack, [])
                in
                res
            | S_LD x ->
                let sn = match (env#get_opnd x) with
                  | Some sn -> sn
                  | None -> failwith (Printf.sprintf "local varible %s doen's exists" x)
                in
                (sn:: stack, [])
            | S_ST x ->
                let s:: stack' = stack in
                let sn = match env#get_opnd x with
                  | None -> env#allocate x
                  | Some sn' -> sn'
                in
                (stack', [X86Mov (s, eax); X86Mov (eax, sn)])
            | S_BINOP s ->
                let y:: x:: stack' = stack in
                let res = env#allocate_local in
                (res:: stack',
                  let (div_op, div_end, mod_end) = (
                      [X86Mov (x, eax); X86Mov (y, ebx); X86Cltd; X86Div ebx],
                      [X86Mov (eax, res)], [X86Mov (edx, res)]) in
                  let (eq_op_start, eq_op_end) = (
                      [X86Xor (eax, eax);
                      X86Mov (x, ebx);
                      X86Mov (y, ecx);
                      X86Cmp (ecx, ebx)], [X86Mov (eax, res)]) in
                  let (prot_put, x', y', prot_load) = ([X86Mov(x, eax); X86Mov(y, ebx)],
                      eax, ebx, [X86Mov (eax, res)]) in
                  match s with
                  | "+" ->
                      prot_put @ [X86Add (y', x')] @ prot_load
                  | "-" ->
                      prot_put @ [X86Sub (y', x')] @ prot_load
                  | "*" ->
                      prot_put @ [X86Mul (y', x')] @ prot_load
                  | "/" ->
                      div_op @ div_end
                  | "%" ->
                      div_op @ mod_end
                  | "<=" ->
                      eq_op_start @ [X86Set (Leq, al)] @ eq_op_end
                  | "<" ->
                      eq_op_start @ [X86Set (Less, al)] @ eq_op_end
                  | "!=" ->
                      eq_op_start @ [X86Set (Neq, al)] @ eq_op_end
                  | ">=" ->
                      eq_op_start @ [X86Set (Geq, al)] @ eq_op_end
                  | ">" ->
                      eq_op_start @ [X86Set (G, al)] @ eq_op_end
                  | "==" ->
                      eq_op_start @ [X86Set (Eq, al)] @ eq_op_end
                  | "&&" ->
                      [
                      X86Xor (eax, eax);
                      X86Mov (x, ebx);
                      X86Mov (y, ecx);
                      X86And (Long, ebx, ebx);
                      X86Set (Nz, al);
                      X86Xor (ebx, ebx);
                      X86And (Long, ecx, ecx);
                      X86Set (Nz, bl);
                      X86And (Long, ebx, eax);
                      X86Mov (eax, res);
                      ]
                  | "!!" ->
                      [
                      X86Xor (eax, eax);
                      X86Mov (x, ebx);
                      X86Mov (y, ecx);
                      X86Or (ebx, ecx);
                      X86Set (Nz, al);
                      X86Mov (eax, res);
                      ]
                )
            
            | S_LABEL s -> (stack, [AsmLabel s])
            | S_JMP s -> (stack, [X86Jmp s])
            | S_COND s ->
                let x:: stack' = stack in
                (stack', [X86Mov (x, ebx); X86And (Long, ebx, ebx); X86Jz s])
            | S_CALL (name, args_n) ->
                call (env#func_name name, args_n) false
            | S_BUILTIN (name, args_n) ->
                call (env#builtin_func_name name, args_n) false
            | S_RET ->
                let x:: stack' = stack in
                (stack', [X86Mov (x, eax); X86Jmp (env#epilogue_label name)])
            | S_ELEM ind ->
                call (env#builtin_func_name "arrget", ind) true
            | S_STA ind ->
                call (env#builtin_func_name "arrset", ind) true
            | S_ARRAY (boxed, ind) ->
                if boxed then
                  call (env#builtin_func_name "arrcreate_boxed", ind) true
                else
                  call (env#builtin_func_name "arrcreate_unboxed", ind) true
          in
          x86code @ compile stack' code'
    in
    compile [] code
  
end

let compile_function env name args body =
  env#get_new_frame;
  env#set_scope FUN;
  List.iteri (fun num_arg arg -> let numb = num_arg + 1 in env#assign_local arg (Farg numb)) args;
  let fun_code = Compile.stack_program env name body in
  let (push_unreserved_regs, pop_unreserved_regs) =
    List.split(List.map (fun arg -> (X86Push arg, X86Pop arg)) (Array.to_list unreserved_regs))
  in
  [AsmLabel (env#func_name name)] @ [Special (Prologue env#allocated_local)] @ push_unreserved_regs @
  fun_code @ [X86Xor (eax, eax)] @
  [AsmLabel (env#epilogue_label name)] @ (List.rev pop_unreserved_regs) @ [Special Epilogue] @ [X86Ret]

let compile_main env body =
  env#get_new_frame;
  env#set_scope GLOB;
  let main_name = env#main_name in
  let fun_code = Compile.stack_program env main_name body in
  [AsmLabel main_name] @ [Special (Prologue env#allocated_local)] @ fun_code @
  [AsmLabel (env#epilogue_label main_name)] @ [X86Xor (eax, eax)] @ [Special Epilogue] @ [X86Ret]

let compile unit =
  let env = new x86env in
  let (defs, main_body) = StackMachine.Compile.unit unit in
  let main_code = compile_main env main_body in
  let funs_code = List.map (fun (name, (args, body)) -> compile_function env name args body) defs in
  let asm = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!) s = !!s; !!"\n" in
  
  !"\t.data";
  List.iteri (fun i x ->
          !(Printf.sprintf "\t%s:\t.asciz\"%s\"" (env#get_str_form i) x))
    env#get_str_consts;
  !"\t.text";
  !"\t.globl\tmain";
  List.iter (fun x ->
          !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#global_vars;
  
  let show_func fun_code = List.iter (fun i -> !(Show.instr i)) fun_code in
  List.iter (fun fun_code -> show_func fun_code) funs_code;
  show_func main_code;
  !!"\n";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  match Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name) with
  | 0 -> ()
  | _ -> failwith "gcc failed with non-zero exit code"

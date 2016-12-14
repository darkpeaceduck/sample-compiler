

module Builtins =
struct
  type t = Int of int | String of bytes | Array of t array
  let to_str = function
    | Int a -> string_of_int(a)
    | String b -> Bytes.to_string(b)
    | _ -> ""
  
  let of_value = function
    | Language.Value.Int a -> Int a
    | Language.Value.String b -> String b
  
  let of_list li boxed =
    Array (Array.of_list (List.map (fun arg ->
                  if boxed then
                    arg
                  else
                    match arg with
                    | Int a -> Int a
                    | _ -> failwith "unboxed with not int value"
            )li))
  let to_bool : t -> bool = function
    | Int a -> a <> 0
    | String b -> (Bytes.length b) > 0
  
  let strset co = function
    | (String s)::(Int i)::(Int c)::[] ->
    Bytes.set s i (Char.chr c);
    (co, String s)
  let strget c = function
    | (String s)::(Int i)::[] -> (c, Int (Char.code(Bytes.get s i)))
  let strmake co = function
    | (Int n)::(Int c)::[]  -> (co, String (Bytes.make n (Char.chr c)))
  let strdup c = function
    | (String s)::[] -> (c, String (Bytes.copy s))
  let strcat c = function
    | (String s1) :: (String s2) :: [] -> (c, String (Bytes.cat s1 s2))
  let strcmp co = function
    | (String s1) :: (String s2) :: [] -> (co, Int (Bytes.compare s1 s2))
  let strlen co = function
    | (String s) :: [] -> (co, Int(Bytes.length s))
  let strsub co = function
    | (String s) :: (Int i) :: (Int l) :: []-> (co, String(Bytes.sub s i l))
  let read (inp, out) _ = 
    let x::inp' = inp in 
    ((inp', out), Int x)
  let write (inp, out) = function
    | x::[] ->
    ((inp, out @ [x]), Int 0)
    
  let invoke name c args = 
    match name with
    | "strset" -> strset c args
    | "strget" -> strget c args
    | "strdup" -> strdup c args
    | "strcat" -> strcat c args
    | "strmake"-> strmake c args
    | "strcmp" -> strcmp c args 
    | "strlen" -> strlen c args
    | "strsub" -> strsub c args
    | "read"   -> read c args
    | "write"  -> write c args
    | _        -> failwith "Builtin not found"
end

module Expr =
struct
  
  open Language.Expr
  open Builtins
  
  let bool2int arg =
    if arg then 1 else 0
  ;;
  
  let invoke_binop str (Int a) (Int b) =
    let result =
      match str with
      | "+" -> a + b
      | "-" -> a - b
      | "*" -> a * b
      | "/" -> a / b
      | "%" -> a mod b
      | "<=" -> bool2int (a <= b)
      | "<" -> bool2int (a < b)
      | "==" -> bool2int (a == b)
      | "!=" -> bool2int (a != b)
      | ">=" -> bool2int (a >= b)
      | ">" -> bool2int (a > b)
      | "&&" -> bool2int (a <> 0 && b <> 0)
      | "!!" -> bool2int (a <> 0 || b <> 0)
    in
    Int result
  ;;
  
  let rec eval ((state, input, output) as c) call_f = function
    | Const n -> (input, output, of_value(n))
    | Var x -> (input, output, state x)
    | Binop (str, a, b) ->
    (* fuckup below probably *)
        let eval' arg input output = eval (state, input, output) call_f arg in
        let (input', output', rc1) = eval' a input output in
        let (input'', output'', rc2) = eval' b input' output' in
        let result = invoke_binop str rc1 rc2 in
        (input'', output'', result)
    | Call (name, args) ->
        let (new_inp, new_outp, (rc, _)) = call_f name args c in
        (new_inp, new_outp, rc)
    | ArrayDef (boxed, list) ->
        let (unboxed_list, input', output') =
          List.fold_left (fun (res, input, output) arg ->
                  let (input', output', rc) = eval (state, input, output) call_f arg in
                  ([rc] @ res, input', output')) ([], input, output) list
        in
        (input', output', (of_list unboxed_list boxed))
end

module MAP = Map.Make(String)

module Stmt =
struct
  
  open Language.Stmt
  open Builtins
  
  let call_ctx_keeper = ref MAP.empty;;
  let add_call_ctx name args body = call_ctx_keeper := MAP.add name (args, body) !call_ctx_keeper;;
  
  let rec call eval name packed_args (state, input, output) =
    let (args, (inp', outp')) =
      List.fold_left (fun (unpacked_args', (inp, outp)) arg ->
              let (e_inp, e_outp, rc) = Expr.eval (state, inp, outp) (call eval) arg in
              ( unpacked_args' @ [rc], (e_inp, e_outp))) ([], (input, output)) packed_args in
    (*Printf.printf "SUKA ";  
    List.iter (fun a-> Printf.printf "arg %s " (to_str a)) args;
    Printf.printf "\n";
    *)
    try
      let (fun_args, stmt) = MAP.find name !call_ctx_keeper in
      let new_state = List.fold_left2 (fun res arg_value fun_arg -> [(fun_arg, arg_value)] @ res)
          [] args fun_args in
      eval new_state inp' outp' stmt
    with
    | Not_found -> 
      let ((inp'', out''), res) = Builtins.invoke name (inp', outp') args in
      (inp'', out'', (res, false))
  
  let rec eval_stmt state input output stmt =
    let rec eval' ((state, input, output, ret) as c) stmt =
      let check_returned (_, _, _, (_, status)) = status in
      if (check_returned c) then
        c
      else
        let retrieve_state (st, _, _, _) = st in
        let state_f c x = List.assoc x (retrieve_state c) in
        let state'= (state_f c) in
        let eval_expr ((_, input, output, _) as c) e = Expr.eval (state_f c, input, output) (call eval_stmt) e in
        let call_with_args name args = call eval_stmt name args (state', input, output) in
        match stmt with
        | Skip -> c
        | Seq (l, r) ->
            let left_unpacked = (eval' c l) in
            if (check_returned left_unpacked) then
              left_unpacked
            else
              eval' left_unpacked r
        | Assign (x, e) ->
            let (e_inp, e_outp, rc) = eval_expr c e in
            ((x, rc) :: state, e_inp , e_outp, ret)
        | If (cond, e1, e2) ->
            let (e_inp, e_out, rc) = eval_expr c cond in
            let selected_expr =
              if to_bool(rc) then e1 else e2
            in
            eval' (state, e_inp, e_out, ret) selected_expr
        | While (cond, e) ->
            let ref_context = ref c in
            while ((check_returned !ref_context) == false &&
              to_bool(let (e_inp, e_outp, rc) = eval_expr !ref_context cond in
                  ref_context := ((retrieve_state !ref_context), e_inp, e_outp, ret);
                  rc )) do
              ref_context := eval' (!ref_context) e;
            done;
            !ref_context
        | Call (name, args) ->
            let (input', output', _) = call_with_args name args in
            (state, input', output', ret)
        | Return e ->
            let (e_inp, e_out, rc) = eval_expr c e in
            (state, e_inp, e_out, (rc, true))
    in
    let (_, input, output, ret) = eval' (state, input, output, (Int 0, false)) stmt in
    (input, output, ret)
  
  let eval_unit input (defs, main_body) =
    List.map (fun (name, (args, body)) -> add_call_ctx name args body) defs;
    let (_, output, _) = eval_stmt [] input [] main_body in
    output
end

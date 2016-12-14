open Language

type i =
  | S_PUSH of Language.Value.t
  | S_LD of string
  | S_ST of string
  | S_BINOP of string
  | S_COND of string
  | S_JMP of string
  | S_LABEL of string
  | S_CALL of string * int
  | S_BUILTIN of string * int
  | S_RET
  | S_ARRAY of bool * int
  | S_ELEM of int
  | S_STA of int

module MAP = Map.Make(String)

module Interpreter =
struct
  open Interpreter.Builtins
  let call_ctx_keeper = ref MAP.empty;;
  let add_call_ctx name args body = call_ctx_keeper := MAP.add name (args, body) !call_ctx_keeper;;
  let get_call_ctx name = MAP.find name !call_ctx_keeper;;
  
  let stack_ctx_keeper = ref [];;
  let push_stack_ctx c = stack_ctx_keeper:= c::!stack_ctx_keeper ;;
  let pop_stack_ctx () = match !stack_ctx_keeper with
    | [] -> failwith "pop from empty stack ctx"
    | x:: xs ->
        stack_ctx_keeper := xs;
        x
  ;;
  
  let map_labels_keepr = ref [];;
  let rec map_labels code' =
    match code' with
    | [] -> ()
    | i:: code'' ->
        match i with
        | S_LABEL s ->
            map_labels_keepr := (s, code'')::!map_labels_keepr;
            map_labels code''
        | _ -> map_labels code''
  
  let rec run (state, stack, input, output) code =
    let get_code_by_label s = try
        List.assoc s !map_labels_keepr
      with
      | Not_found -> failwith "HU" in
    let rec run' ((state, stack, input, output) as c) code =
      let rec call_get_state stack args =
        match args with
        | [] -> ([], stack)
        | y:: args' ->
            let x:: stack' = stack in
            let (state, stack'') = call_get_state stack' args' in
            ((y, x) :: state, stack'')
      in
      let call ((state, stack, input, output), code) args body =
        let (state', stack) = call_get_state stack args in
        push_stack_ctx (state, stack, code);
        run (state', stack, input, output) body
      in
      let ret input output value =
        match !stack_ctx_keeper with
        | [] -> output
        | _ ->
            let (state, stack, body) = pop_stack_ctx() in
            run (state, value:: stack, input, output) body
      in
      let rec split i y x' =
        match x' with
        | [] -> if i == 0 then (y, [] ) else failwith "wrong argument"
        | x:: xs -> if i == 0 then (y, x:: xs) else split (i -1) (y @ [x]) xs
      in
      match code with
      | [] -> output
      | i:: code' ->
          match i with
          | S_PUSH n ->
              run' (state, of_value(n):: stack, input, output) code'
          | S_LD x ->
              run' (state, (List.assoc x state):: stack, input, output) code'
          | S_ST x ->
              let y:: stack' = stack in
              run' ((x, y):: state, stack', input, output) code'
          | S_BINOP s ->
              let b:: a:: stack' = stack in
              let invoke_res = Interpreter.Expr.invoke_binop s a b in
              run' (state, invoke_res:: stack', input, output) code'
          | S_JMP s ->
              run' c (get_code_by_label s)
          | S_LABEL s ->
              run' c code'
          | S_COND s ->
              let a:: stack' = stack in
              if to_bool(a) == false then
                run' (state, stack', input, output) (get_code_by_label s)
              else run' (state, stack', input, output) code'
          | S_CALL (name, n_args) ->
              let (args, body) = get_call_ctx name in
              call (c, code') (List.rev args) body
          | S_BUILTIN (name, n_args) ->
          
              let (args, stack') = split (n_args) [] stack in
              let ((input', output'), res) = invoke name (input, output) (List.rev args) in
              run' (state, res:: stack', input', output') code'
          | S_RET ->
              let a:: stack' = stack in
              ret input output a
          | S_ARRAY (boxed, n_args) ->
              let (args, stack') = split (n_args) [] stack in
              let ar = of_list (List.rev args) boxed in
              run' (state, ar:: stack', input, output) code'
          | S_ELEM (n_args) ->
              let (args, stack') = split (n_args) [] stack in
              let res = arrget (List.rev args) in
              run' (state, res:: stack', input, output) code'
          | S_STA (n_args) ->
              let (args, stack') = split (n_args) [] stack in
              let res = arrset (List.rev args) in
              run' (state, res:: stack', input, output) code'
    in
    run' (state, stack, input, output) code
  
  let run_unit input (defs, main_body) =
    map_labels main_body;
    List.iter (fun (_, (_, body)) ->
            map_labels body) defs;
    List.map (fun (name, (args, body)) -> add_call_ctx name args body) defs;
    run ([], [], input, []) main_body
  ;;
end

module Compile =
struct
  
  open Language.Expr
  open Language.Stmt
  open Language.Def
  open Language.Unit
  
  let label_counter = ref 0;;
  let label_generate () = label_counter:=!label_counter + 1; "label"^(string_of_int(!label_counter)) ;;
  
  let unfold_list_expr exec args =
    List.fold_left (fun res arg -> res @ exec arg) [] args
  
  let rec expr = function
    | Var x -> [S_LD x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]
    | Call (s, args) ->
        unfold_list_expr expr args @ [S_CALL (s, (List.length args))]
    | ArrayDef (boxed, args) ->
        unfold_list_expr expr args @ [S_ARRAY (boxed, List.length args)]
    
    | ArrayImp (ar, args) ->
        expr ar @ unfold_list_expr expr args @ [S_ELEM ((List.length args) +1)]
  
  let rec stmt = function
    | Skip -> []
    | Assign (x, e) -> expr e @ [S_ST x]
    | Seq (l, r) -> stmt l @ stmt r
    | If (e, s1, s2) ->
        let label_start_second = label_generate() in
        let label_end_second = label_generate() in
        expr e @ [S_COND (label_start_second)] @ stmt s1 @ [S_JMP (label_end_second)] @
        [S_LABEL (label_start_second)] @ stmt s2 @ [S_LABEL (label_end_second)]
    | While (e, s) ->
        let label_cond = label_generate() in
        let label_end = label_generate() in
        [S_LABEL label_cond] @ expr e @ [S_COND label_end] @ stmt s @ [S_JMP label_cond] @ [S_LABEL label_end]
    | Return e ->
        expr e @ [S_RET]
    | Call (s, args) ->
        List.fold_left (fun res arg -> res @ expr arg) [] args @ [S_CALL (s, (List.length args))]
    
    | ArrayAssign (ar, args, e) ->
        expr ar @ unfold_list_expr expr args @ expr e @ [S_STA ((List.length args) + 2)]
  
  let rec retrive_builtins defs = function
    | [] -> []
    | x:: xs ->
        let replac =
          match x with
          | S_CALL(s, args) ->
              if List.exists (fun (a, _) -> String.equal s a) defs then
                S_CALL(s, args)
              else
                S_BUILTIN(s, args)
          | e -> e
        in
        [replac] @ retrive_builtins defs xs
  ;;
  
  let rec def = function
    | (name, (args, body)) -> (name, (args, stmt body))
  
  let rec unit (defs, main_body) =
    let defs_compile = List.fold_left (fun res d -> [def d] @ res) [] defs in
    let defs_with_builtins = List.map (fun (name, (args, body)) ->
              let new_body = retrive_builtins defs_compile body in
              (name, (args, new_body))) defs_compile in
    (defs_with_builtins, retrive_builtins defs_compile (stmt main_body))
  ;;
end

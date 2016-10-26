type i =
| S_READ
| S_WRITE
| S_PUSH  of int
| S_LD    of string
| S_ST    of string
| S_BINOP of string
| S_COND  of string
| S_JMP   of string
| S_LABEL of string
| S_CALL  of string * int
| S_RET

module Interpreter =
  struct
    let run input code =
			let rec map_labels l code' = 
				match code' with
				| [] -> l
				| i::code'' ->
					match i with 
					| S_LABEL s -> map_labels ((s, code'')::l) code''
				  | _ -> map_labels l code''
			in 
			let label_map = map_labels [] code in
			let get_code_by_label s = (List.assoc s label_map) in
      let rec run' ((state, stack, input, output) as c) code =
		  	match code with
					| []       -> output
					| i::code' ->
				    match i with
	              | S_READ ->
										  let y::input' = input in
					  						run' (state, y::stack, input', output) code'
	              | S_WRITE ->
										  let y::stack' = stack in
					  						run' (state, stack', input, output @ [y]) code'
	              | S_PUSH n ->
					 						run' (state, n::stack, input, output) code'
	              | S_LD x ->
					 						run' (state, (List.assoc x state)::stack, input, output) code'
	              | S_ST x ->
				 							 let y::stack' = stack in
										  run' ((x, y)::state, stack', input, output) code'
	              | S_BINOP s ->
											let b::a::stack' = stack in
											let invoke_res = Interpreter.Expr.invoke_binop s a b in
											run' (state, invoke_res::stack', input, output) code'
								| S_JMP s ->
											run' c (get_code_by_label s)
								| S_LABEL s ->
											run' c code'
								| S_COND s ->
											let a::stack' = stack in
											if a == 0 then run' c (get_code_by_label s) else run' (state, stack', input, output) code'
											
 			in
      run' ([], [], input, []) code
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt
		open Language.Def
		open Language.Unit
		
		let label_counter = ref 0;;
		let label_generate ()= label_counter:=!label_counter + 1; "label"^(string_of_int(!label_counter)) ;;
		 
			
    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]
		| Call (s, args) ->
			List.fold_left (fun res arg -> res @ expr arg) [] args @   [S_CALL (s, (List.length args))]
	  

    let rec stmt = function
    | Skip          -> []
    | Assign (x, e) -> expr e @ [S_ST x]
    | Read    x     -> [S_READ; S_ST x]
    | Write   e     -> expr e @ [S_WRITE]
    | Seq    (l, r) -> stmt l @ stmt r
		| If  (e, s1, s2) -> 
			let label_start_second = label_generate() in
			let label_end_second = label_generate() in 
				expr e @ [S_COND (label_start_second)] @ stmt s1 @ [S_JMP (label_end_second)] @ 
				[S_LABEL (label_start_second)] @ stmt s2 @ [S_LABEL (label_end_second)]
		| While (e, s) ->
			let label_cond = label_generate() in
			let label_end = label_generate() in
			[S_LABEL label_cond] @ expr e @ [S_COND label_end] @ stmt s @ [S_JMP label_cond] @ [S_LABEL label_end] 
		| Return e->
			expr e @ [S_RET]
		| Call (s, args) ->
			List.fold_left (fun res arg -> res @ expr arg) [] args @   [S_CALL (s, (List.length args))]
			
		
		let rec def = function
			| (name, (args, body)) -> (name, (args, stmt body))

		let rec unit (defs, main_body) = 
			let defs_compile = List.fold_left (fun res d -> [def d] @ res) [] defs in
			(defs_compile, stmt main_body)
		;;
  end

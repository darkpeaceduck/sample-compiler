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

module Interpreter =
  struct

    let run input code =
      let rec run' (state, stack, input, output) code =
		  	match code with
					| []       -> output
					| i::code' ->
				    run' (match i with
	              | S_READ ->
										  let y::input' = input in
					  						(state, y::stack, input', output)
	              | S_WRITE ->
										  let y::stack' = stack in
					  						(state, stack', input, output @ [y])
	              | S_PUSH n ->
					 						(state, n::stack, input, output)
	              | S_LD x ->
					 						 (state, (List.assoc x state)::stack, input, output)
	              | S_ST x ->
				 							 let y::stack' = stack in
										  ((x, y)::state, stack', input, output)
	              | S_BINOP s ->
											let b::a::stack' = stack in
											let invoke_res = Interpreter.Expr.invoke_binop s a b in
											(state, invoke_res::stack', input, output)
	              )
      		code'
 			in
      run' ([], [], input, []) code
  end

module Compile =
  struct

    open Language.Expr
    open Language.Stmt
		
		let label_counter = ref 0;;
		let label_generate ()= label_counter:=!label_counter + 1; "label"^(string_of_int(!label_counter)) ;;

    let rec expr = function
    | Var   x -> [S_LD   x]
    | Const n -> [S_PUSH n]
    | Binop (s, x, y) -> expr x @ expr y @ [S_BINOP s]

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

  end

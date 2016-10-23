module Expr =
  struct

    open Language.Expr
		
		let bool2int arg = 
			if arg then 1 else 0
		;;
		
		let invoke_binop str a b =  
			match str with
			| "+" -> a + b
			| "-" -> a - b
			| "*" -> a * b
			| "/" -> a / b
			| "%" -> a mod b
			| "<=" -> bool2int (a <= b)
			| "<"  -> bool2int (a < b)
			| "==" -> bool2int (a == b)
			| "!=" -> bool2int (a != b)
			| ">=" -> bool2int (a >= b)
			| ">"  -> bool2int (a > b)
			| "&&" -> bool2int (a <> 0  && b <> 0)
			| "!!" -> bool2int (a <> 0  || b <> 0)
		;;

    let rec eval ((state, input, output) as c) call_f = function
    | Const  n -> (input, output, n)
    | Var    x -> (input, output, state x)
    | Binop  (str, a, b) -> 
			let eval' arg input output = eval c call_f arg in
			let (input', output', rc1) = eval' a input output in
			let (input'', output'', rc2) = eval' b input' output' in 
			let result = invoke_binop str rc1 rc2 in
			(input'', output'', result)
		| Call (name, args) ->
			let (new_inp, new_outp, (rc, _)) = call_f name args c in
				(new_inp, new_outp, rc)
  	end
  
module MAP = Map.Make(String)

module Stmt =
  struct

    open Language.Stmt
		
		let call_ctx_keeper = ref MAP.empty;;
    let add_call_ctx name args body = call_ctx_keeper := MAP.add name (args, body) !call_ctx_keeper;;

		let rec call eval name packed_args (state, input, output) = 
			let (args, (inp', outp')) = 
					List.fold_left (fun (unpacked_args', (inp, outp)) arg -> 
					let  (e_inp, e_outp, rc) = Expr.eval (state, inp, outp) (call eval) arg in
					([rc] @ unpacked_args', (e_inp, e_outp))) ([], (input, output)) packed_args
			in 
			let (fun_args, stmt) = MAP.find name !call_ctx_keeper in
			let new_state = List.fold_left2 (fun res arg_value fun_arg -> [(fun_arg, arg_value)] @ res) 
			[] args fun_args in
				eval new_state inp' outp' stmt
				
    let rec eval_stmt state input output stmt =
      let rec eval' ((state, input, output, ret) as c) stmt =
				let check_returned (_,_,_,(_,status)) = status in
				if (check_returned c) then
					c
				else
				let retrieve_state (st, _, _, _) = st in
				let state_f c x = List.assoc x (retrieve_state c) in
				let state'= (state_f c) in
				let eval_expr ((_, input, output, _) as c) e = Expr.eval (state_f c, input, output) (call eval_stmt) e in
				let call_with_args name args = call eval_stmt name args (state', input, output) in
				match stmt with
					| Skip          -> c
					| Seq    (l, r) -> 
						let left_unpacked = (eval' c l) in 
						if (check_returned left_unpacked) then
							left_unpacked
						else
							eval' left_unpacked r
					| Assign (x, e) -> 
						let (e_inp, e_outp, rc) = eval_expr c e in
						  ((x, rc) :: state, e_inp , e_outp, ret)
					| Write   e     ->
						let (e_inp, e_outp, rc) = eval_expr c e in
						 (state, e_inp, e_outp @ [rc], ret)
					| Read    x     ->
					    let y::input' = input in
					    ((x, y) :: state, input', output, ret)
					| If (cond, e1, e2) ->
						let (e_inp, e_out, rc)  = eval_expr c cond in 
						let selected_expr =  
							if rc <> 0 then e1 else e2
						in
						eval' (state, e_inp, e_out, ret) selected_expr
					| While (cond, e) -> 
						let ref_context = ref c in
						while ((check_returned !ref_context) == false  && 
							(let (e_inp, e_outp, rc) = eval_expr !ref_context cond in
							ref_context := ((retrieve_state !ref_context), e_inp, e_outp, ret);
							rc ) <> 0)  do
							ref_context := eval' (!ref_context) e;
						done; 
						!ref_context
					| Call (name, args)  -> 
						let (input', output', _) = call_with_args name args  in
						(state, input', output', ret)
					| Return e -> 
						let (e_inp, e_out, rc)  = eval_expr c e in
						(state, e_inp, e_out, (rc, true))
 			in
		  let (_, input, output, ret) = eval' (state, input, output, (0, false)) stmt in
		  (input, output, ret)
			
		let eval_unit input (defs, main_body) = 
			List.map (fun (name, (args, body)) -> add_call_ctx name args body) defs;
			let (_, output, _) = eval_stmt [] input [] main_body in 
			output
  end

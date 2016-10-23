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
		;;

    let rec eval state = function
    | Const  n -> n
    | Var    x -> state x
    | Binop  (str, a, b) -> invoke_binop str (eval state a) (eval state b)
  	end
  
module Stmt =
  struct

    open Language.Stmt

    let eval input stmt =
      let rec eval' ((state, input, output) as c) stmt =
				let state_f (st, _ , _) x = List.assoc x st in
				let state'= (state_f c) in
				match stmt with
					| Skip          -> c
					| Seq    (l, r) -> eval' (eval' c l) r
					| Assign (x, e) -> ((x, Expr.eval state' e) :: state, input, output)
					| Write   e     -> (state, input, output @ [Expr.eval state' e])
					| Read    x     ->
					    let y::input' = input in
					    ((x, y) :: state, input', output)
					| If (cond, e1, e2) -> 
						let selected_expr = 
							if (Expr.eval state' cond) <> 0 then e1 else e2
						in
						eval' c selected_expr
					| While (cond, e) -> 
						let ref_context = ref c in
						while (Expr.eval (state_f !ref_context) cond) <> 0 do
							ref_context := eval' (!ref_context) e
						done; 
						!ref_context
 			in
		  let (_, _, result) = eval' ([], input, []) stmt in
		  result

  end

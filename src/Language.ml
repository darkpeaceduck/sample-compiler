open Ostap 
open Matcher

module Expr =
  struct

    type t =
    | Const of int
    | Var   of string
    | Binop of string * t * t

		let rec is_zero x = 
			Binop ("==", x, Const 0) 
		;;

     ostap (
      parse:
				l:eqs suf:(("&&") eqs)* {
					List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
				}
			| eqs;
				
			eqs:
        l:addi suf:(("<=" | "<" | "==" | "!=" | ">=" | ">") addi)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | addi;

      addi:
        l:mulli suf:(("+" | "-") mulli)* {
          List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | mulli;

      mulli:
        l:primary suf:(("*" | "/" | "%") primary)* {
           List.fold_left (fun l (op, r) -> Binop (Token.repr op, l, r)) l suf
        }
      | primary;

      primary:
        n:DECIMAL {Const n}
      | x:IDENT   {Var   x}
      | -"(" parse -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Read   of string
    | Write  of Expr.t
    | Assign of string * Expr.t
    | Seq    of t * t
		| If     of Expr.t * t * t
		| While  of Expr.t * t

    ostap (
      parse: s:simple d:(-";" parse)? {
	match d with None -> s | Some d -> Seq (s, d)
      };
      simple:
        x:IDENT ":=" e:!(Expr.parse)     {Assign (x, e)}
      | %"read"  "(" x:IDENT ")"         {Read x}
      | %"write" "(" e:!(Expr.parse) ")" {Write e}
      | %"skip"                          {Skip}
			| %"if" cond:!(Expr.parse) "then" e1:parse elseif_st:(-"elif" cond:!(Expr.parse) -"then" parse)* else_st:(-"else" parse)? "fi" {
				let e2 = 	match else_st with
				| None -> Skip
				| Some d -> d
				in
				List.fold_right (fun (cond,e) r -> If (cond, e, r)) ((cond,e1)::elseif_st) e2
			}
			| %"while" cond:!(Expr.parse) "do" e:parse "od" { While (cond, e)}
			| %"for" e1:parse "," cond:!(Expr.parse) "," e2:parse "do" e3:parse "od" { Seq ( e1, While(cond, Seq (e3, e2) ) ) }  
			| %"repeat" e:parse "until" cond:!(Expr.parse) { Seq (e, While((Expr.is_zero cond), e))}
    )

  end

open Ostap
open Matcher


module Value =
  struct
     type t = Int of int | String of bytes
     
    let to_string : t -> string = function
      | Int a -> string_of_int a
      | String b -> Bytes.to_string(b)
      ostap (
        parse:
        s: STRING { String (String.sub s 1 (String.length s - 2)) }
        | c: CHAR { Int (Char.code c) } 
        | x : DECIMAL { Int x }
        | "true" { Int 1 }
        | "false" { Int 0 }
      )
  end
  
  
module Expr =
  struct
    type t =
    | Const of Value.t
    | Var of string
    | Binop of string * t * t
    | Call of string * t list
    | ArrayDef of bool * t list
    | ArrayImp of t * t list

    let rec is_zero x = 
      Binop ("==", x, Const (Int 0)) 
    ;;
    ostap (
      parse:
	  !(Ostap.Util.expr 
             
            (fun x -> x)
	     
      
      (Array.map 
      
      (fun (a, s) -> a, 
                         List.map (fun s -> ostap(- $(s)), (fun x y -> Binop (s, x, y))) s
       ) 
              [| 
		`Lefta, ["!!"];
		`Lefta, ["&&"];
		`Nona , ["=="; "!="; "<="; "<"; ">="; ">"];
		`Lefta, ["+" ; "-"];
		`Lefta, ["*" ; "/"; "%"];
              |] 
	     )
	     primary);
      
      call_or_var:
         f: IDENT args: (-"(" !(Util.list0 parse) -")")? {
    	  match args with 
    	  | None -> Var f 
    	  | Some args -> Call (f, args)
            } ;
            
      inline_array:
         "[" args:!(Util.list0 parse) "]" { ArrayDef (false, args) }
         | "{" args:!(Util.list0 parse) "}" { ArrayDef (true, args) };
        
      array_imp_arg : inline_array | call_or_var;
      
      array_imp : ar: array_imp_arg args: (-"[" parse -"]") + { ArrayImp (ar, args) };
      
      const : n:!(Value.parse) { Const n };

      primary:
        const
      | array_imp
      | call_or_var
      | inline_array
      | -"(" parse -")"
  )
  end

module Stmt =
  struct

    type t =
    | Skip
    | Assign of string * Expr.t
    | Seq of t * t
    | If of Expr.t * t * t
    | While of Expr.t * t
    | Call of string * Expr.t list
    | Return of Expr.t
    | ArrayAssign of Expr.t * Expr.t list * Expr.t

    ostap (
      parse: s: simple d: (-";" parse)? { match d with None -> s | Some d -> Seq (s, d) };
      expr : !(Expr.parse);
      simple:
        x: IDENT s: (":=" e: expr { Assign (x, e) } | 
                   "(" args:!(Util.list0 expr) ")" { Call (x, args) }
                  ) { s }
      | %"skip" { Skip }
      | %"return" e: expr { Return e } 
      | %"if" e: expr 
	  %"then" the: parse 
          elif: (%"elif" expr %"then" parse) *
	  els: (%"else" parse)? 
        %"fi" {
          If (e, the, 
	         List.fold_right 
		   (fun (e, t) elif -> If (e, t, elif)) 
		   elif
		   (match els with None -> Skip | Some s -> s)
          )
        }
      | %"while" e: expr %"do" body: parse %"od" { While (e, body) }
      | %"repeat" e: parse "until" cond:!(Expr.parse) { Seq (e, While((Expr.is_zero cond), e)) }
      | %"for" i: parse "," c: expr "," s: parse %"do" b: parse %"od" {
	  Seq (i, While (c, Seq (b, s)))
        }
        
      | ar :!(Expr.array_imp_arg) args: (-"[" !(Expr.parse) -"]") + ":=" e:!(Expr.parse)
       { ArrayAssign (ar, args, e) }
    )

  end

module Def =
  struct

    type t = string * (string list * Stmt.t)

    ostap (
      arg : IDENT;
      parse: %"fun" name: IDENT "(" args:!(Util.list0 arg) ")" %"begin" body:!(Stmt.parse) %"end" {
        (name, (args, body))
      }
    )

  end

module Unit =
  struct

    type t = Def.t list * Stmt.t

    ostap (
      parse: !(Def.parse) * !(Stmt.parse)
    )

  end

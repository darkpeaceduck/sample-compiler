open Ostap
open Matcher


module Value =
  struct
     type t = Int of int | String of bytes
     
     let to_bool : t -> bool = function
      | Int a -> a <> 0
      | String b -> (Bytes.length b) > 0
     let to_string : t -> string = function
      | Int a -> string_of_int a
      | String b -> Bytes.to_string(b)  
      ostap (
        parse:
        s:STRING   { String (String.sub s 1 (String.length s - 2)) }
        | c:CHAR { Int (Char.code c)} 
        | x : DECIMAL { Int x}
        | "true"  { Int 1}
        | "false" { Int 0}
      )
  end
  
  
module Expr =
  struct
    type t =
    | Const of Value.t
    | Var of string
    | Binop of string * t * t
    | Call of string * t list

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
      primary:
        n:!(Value.parse) { Const n }
      | f: IDENT args: (-"(" !(Util.list0 parse) -")")? {
	  match args with 
	  | None -> Var f 
	  | Some args -> Call (f, args)
        } 
      | -"(" parse -")"
    )

  end

module Stmt =
  struct

    type t =
    | Skip
    | Assign of string * Expr.t
    | Read of string
    | Write of Expr.t
    | Seq of t * t
    | If of Expr.t * t * t
    | While of Expr.t * t
    | Call of string * Expr.t list
    | Return of Expr.t

    ostap (
      parse: s: simple d: (-";" parse)? { match d with None -> s | Some d -> Seq (s, d) };
      expr : !(Expr.parse);
      simple:
        x: IDENT s: (":=" e: expr { Assign (x, e) } | 
                   "(" args:!(Util.list0 expr) ")" { Call (x, args) }
                  ) { s }
      (*| %"read"  "(" x:IDENT ")" {Read x}
      | %"write" "(" e: expr ")" { Write e }
      *)
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

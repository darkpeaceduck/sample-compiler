open Ostap

let parse infile =
  let s = Util.read infile in
  Util.parse
    (object
       inherit Matcher.t s
       inherit Util.Lexers.ident [
         "skip"; "if"; "else"; "elif"; "fi";
         "while"; "do"; "od"; "repeat"; "until";
         "for"; "fun"; "return"
       ] s
       inherit Util.Lexers.decimal s
       inherit Util.Lexers.char s
       inherit Util.Lexers.string s
       inherit Util.Lexers.skip [
	 Matcher.Skip.whitespaces " \t\n";
	 Matcher.Skip.lineComment "--";
	 Matcher.Skip. nestedComment "(*" "*)"
       ] s
     end
    )
    (ostap (!(Language.Unit.parse) - EOF))

let main = ()
  try
    let mode, filename =
      match Sys.argv.(1) with
      | "-s" -> `SM , Sys.argv.(2)
      | "-o" -> `X86, Sys.argv.(2)
      | "-i" -> `Int, Sys.argv.(2)
      | _ -> raise (Invalid_argument "invalid flag")
    in
    match parse filename with
    | `Ok stmt -> 
	(match mode with
	 | `X86 ->
             let basename = Filename.chop_suffix filename ".expr" in 
	     X86.build stmt basename
	 | _ -> 
	     let rec read acc =
	       try
		 let r = read_int () in
		 Printf.printf "> ";
		 read (acc @ [r]) 
               with End_of_file -> acc
	     in
	     let input = read [] in
	     let output =
	       match mode with
	       | `SM -> 
          StackMachine.Interpreter.run_unit input (StackMachine.Compile.unit stmt)
	       | _ -> 
          Interpreter.Stmt.eval_unit input stmt
	     in
	     List.iter (fun i -> Printf.printf "%s\n" (Interpreter.Builtins.to_str(i))) output
	)

    | `Fail er -> Printf.eprintf "errored %s\n" er
  with 
  | Invalid_argument er ->
      Printf.printf "INVALID : %s" er;
      Printf.printf "Usage: rc.byte <command> <name.expr>\n";
      Printf.printf "  <command> should be one of: -i, -s, -o\n"

type opnd = R of int | R8 of int | S of int | M of string | L of int

let x86regs = [|
  "%eax"; 
  "%ebx"; 
  "%ecx"; 
  "%edx"; 
  "%esi"; 
  "%edi"
|]

let x86regs8 = [|
	"%al";
	"%ah";
	"%bl";
|]
	
let num_of_regs = Array.length x86regs
let word_size = 4

let eax = R 0
let ebx = R 1
let ecx = R 2
let edx = R 3
let esi = R 4
let edi = R 5

let al  = R8 0
let ah  = R8 1
let bl  = R8 2

type cmp_suf = 
	| Eq | Neq | L | Leq | G | Geq | Nz

type instr_suf = 
	| L | None


type instr =
| X86Add  of opnd * opnd
| X86Sub  of opnd * opnd
| X86Mul  of opnd * opnd
| X86Xor  of opnd * opnd
| X86Or   of opnd * opnd
| X86Div  of opnd
| X86Cmp  of opnd * opnd
| X86Cltd
| X86Mov  of opnd * opnd
| X86Push of opnd
| X86Pop  of opnd
| X86Ret
| X86Call of string
| X86Set  of cmp_suf * opnd
| X86And  of instr_suf * opnd * opnd
| AsmLabel of string
| X86Jmp of string
| X86Jz  of string

module S = Set.Make (String)

class x86env =
  object(self)
    val    local_vars = ref S.empty
    method local x    = local_vars := S.add x !local_vars
    method local_vars = S.elements !local_vars

    val    allocated  = ref 0
    method allocate n = allocated := max n !allocated
    method allocated  = !allocated
  end


(* S _ operands only allocates from stack lol *) 
let allocate env stack =
  match stack with
  | []                              -> env#allocate 1; S 1
  | (S n)::_                        -> env#allocate (n+1); S (n+1)
  | _                               -> failwith "wrong operand on stack"

module Show =
  struct

    let opnd = function
    | R i -> x86regs.(i)
		| R8 i -> x86regs8.(i)
    | S i -> Printf.sprintf "-%d(%%ebp)" (i * word_size)
    | M x -> x
    | L i -> Printf.sprintf "$%d" i

		let cmp_suf = function
			| Eq -> "e"
			| Neq -> "ne"
			| L -> "l"
			| Leq -> "le"
			| G -> "g"
			| Geq -> "ge"
			| Nz  -> "nz"

		let instr_suf = function
			| L -> "l"
			| None -> ""

    let instr = function
    | X86Add (s1, s2) -> Printf.sprintf "\taddl\t%s,\t%s"  (opnd s1) (opnd s2)
		| X86Sub (s1, s2) -> Printf.sprintf "\tsubl\t%s,\t%s"  (opnd s1) (opnd s2)
		| X86Cmp (s1, s2) -> Printf.sprintf "\tcmp\t%s,\t%s"   (opnd s1) (opnd s2)
		| X86Xor (s1, s2) -> Printf.sprintf "\txorl\t%s,\t%s"    (opnd s1) (opnd s2)
		| X86Or (s1, s2) -> Printf.sprintf "\torl\t%s,\t%s"    (opnd s1) (opnd s2)
    | X86Mul (s1, s2) -> Printf.sprintf "\timull\t%s,\t%s" (opnd s1) (opnd s2)
		| X86Div s        -> Printf.sprintf "\tidivl\t%s"       (opnd s) 
    | X86Mov (s1, s2) -> Printf.sprintf "\tmovl\t%s,\t%s"  (opnd s1) (opnd s2)
    | X86Push s       -> Printf.sprintf "\tpushl\t%s"      (opnd s )
    | X86Pop  s       -> Printf.sprintf "\tpopl\t%s"       (opnd s )
    | X86Ret          -> "\tret"
    | X86Call p       -> Printf.sprintf "\tcall\t%s" p
		| X86Cltd         -> "\tcltd"
		| X86Set (suf, p)       -> Printf.sprintf "\tset%s\t%s" (cmp_suf suf) (opnd p)
		| X86And (suf, s1, s2)  -> Printf.sprintf "\tand%s\t%s,\t%s" (instr_suf suf) (opnd s1) (opnd s2)
		| AsmLabel s            -> Printf.sprintf "\t%s:" s
		| X86Jmp s        ->  Printf.sprintf "\tjmp %s" s
		| X86Jz s        ->  Printf.sprintf "\tjz %s" s

  end

module Compile =
  struct

    open StackMachine
		

    let stack_program env code =
      let rec compile stack code =
	match code with
	| []       -> []
	| i::code' ->
	    let (stack', x86code) =
              match i with
              | S_READ   -> 
								let s = allocate env stack in
								(s::stack, [X86Call "read"; X86Mov (eax, s)])
              | S_WRITE  -> 
								let x::stack' = stack in ( stack, [
														X86Mov (x, eax);
														X86Push (eax); 
														X86Call "write"; 
														X86Pop (eax)])
              | S_PUSH n ->
								  let s = allocate env stack in
								  (s::stack, [X86Mov (L n, eax); X86Mov (eax, s)])
              | S_LD x   ->
								  env#local x;
								  let s = allocate env stack in
								  (s::stack, [X86Mov (M x, eax); X86Mov (eax, s)])
              | S_ST x   ->
								  env#local x;
								  let s::stack' = stack in
								  (stack', [X86Mov (s, eax); X86Mov (eax, M x)])
				      | S_BINOP s ->
								let y::x::stack' = stack in
										(x::stack', 
											let (div_op, div_end, mod_end) = ([X86Mov (x, eax); X86Cltd; X86Div y], [X86Mov (eax, x)], [X86Mov (edx, x)]) in
											let (eq_op_start, eq_op_end) = ( [X86Xor (eax, eax); X86Mov (x, ebx); X86Mov (y, ecx); 
													X86Cmp (ecx, ebx)], [X86Mov (eax, x)]) in
											let (prot_put, x', prot_load) = ([X86Mov(x, eax)], eax, [X86Mov (eax, x)]) in
											match s with 
												| "+" -> 
													prot_put @ [X86Add (y, x')] @ prot_load
												| "-" -> 
													prot_put @ [X86Sub (y, x')] @ prot_load
												| "*" ->
													prot_put @ [X86Mul (y, x')] @ prot_load
												| "/" ->
													div_op @ div_end
												| "%" ->
													div_op @ mod_end
												| "<=" ->
													eq_op_start @ [X86Set (Leq, al)] @ eq_op_end
												| "<" -> 
													eq_op_start @ [X86Set (L, al)] @ eq_op_end
												| "!=" ->
													eq_op_start @ [X86Set (Neq, al)] @ eq_op_end 
												| ">=" ->
													eq_op_start @ [X86Set (Geq, al)] @ eq_op_end
												| ">" ->
													eq_op_start @ [X86Set (G, al)] @ eq_op_end
												| "==" -> 
													eq_op_start @ [X86Set (Eq, al)] @ eq_op_end
												| "&&" ->
													[
													 X86Xor (eax, eax);
													 X86Mov (x, ebx);
													 X86Mov (y, ecx);
													 X86And (L, ebx, ebx); 
													 X86Set (Nz, al);
													 X86Xor (ebx, ebx);
													 X86And (L, ecx, ecx); 
													 X86Set (Nz, bl);
													 X86And (L, ebx, eax); 
   												 X86Mov (eax, x);
													]
												| "!!" -> 
													[
													 X86Xor (eax, eax);
													 X86Mov (x, ebx);
													 X86Mov (y, ecx);
													 X86Or (ebx, ecx); 
													 X86Set (Nz, al);
   												 X86Mov (eax, x);
													]
										)

							| S_LABEL s -> (stack, [AsmLabel s])
							| S_JMP   s -> (stack, [X86Jmp s])
							| S_COND  s -> 
								let x::stack' = stack in 
								(stack', [X86Mov (x, ebx); X86And (L, ebx, ebx); X86Jz s]) 
										  
	    in
	    x86code @ compile stack' code'
      in
      compile [] code

  end

let compile stmt =
  let env = new x86env in
  let code = Compile.stack_program env @@ StackMachine.Compile.stmt stmt in
  let asm  = Buffer.create 1024 in
  let (!!) s = Buffer.add_string asm s in
  let (!)  s = !!s; !!"\n" in
  !"\t.text";
  List.iter (fun x ->
      !(Printf.sprintf "\t.comm\t%s,\t%d,\t%d" x word_size word_size))
    env#local_vars;
  !"\t.globl\tmain";
  let prologue, epilogue =
    if env#allocated = 0
    then (fun () -> ()), (fun () -> ())
    else
      (fun () ->
         !"\tpushl\t%ebp";
         !"\tmovl\t%esp,\t%ebp";
         !(Printf.sprintf "\tsubl\t$%d,\t%%esp" (env#allocated * word_size))
      ),
      (fun () ->
         !"\tmovl\t%ebp,\t%esp";
         !"\tpopl\t%ebp"
      )
  in
  !"main:";
  prologue();
  List.iter (fun i -> !(Show.instr i)) code;
  epilogue();
  !"\txorl\t%eax,\t%eax";
  !"\tret";
  Buffer.contents asm

let build stmt name =
  let outf = open_out (Printf.sprintf "%s.s" name) in
  Printf.fprintf outf "%s" (compile stmt);
  close_out outf;
  ignore (Sys.command (Printf.sprintf "gcc -m32 -o %s $RC_RUNTIME/runtime.o %s.s" name name))

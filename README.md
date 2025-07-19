# Return Oriented Programming Language

Language inspired by ROP concept with mix of assembly, C.

## Targets

- Any target that is supported by C
- WASM

## Grammar

```ebnf

program = { gadget_def } , stack_init ;

(* Gadgets are the fundamental building blocks *)
gadget_def = "gadget" , identifier , ":" , gadget_body ;

gadget_body = { instruction } , return_stmt ;

(* Instructions can only manipulate data, not control flow *)
instruction = assignment
            | call
            | stack_op
            | arithmetic
            | memory_op
            | conditional_mod
            ;

(* Assignments modify registers/variables *)
assignment = identifier , "=" , expression ;

(* Stack operations - the heart of ROP *)
stack_op = "push" , expression
         | "pop" , identifier
         | "peek" , identifier , "[" , expression "]"  (* peek at stack offset *)
         | "stack_swap" , expression , expression      (* swap stack positions *)
         ;

(* Arithmetic operations *)
arithmetic = identifier , arith_op , "=" , expression , arith_op , expression ;
arith_op = "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" ;

(* Memory operations *)
memory_op = "store" , expression , expression  (* store value at address *)
          | "load" , identifier , expression   (* load from address *)
          ;

(* Conditional modifications - can only affect data, not flow *)
conditional_mod = "if" , condition , "then" , identifier , "=" , expression ;

condition = expression , comp_op , expression ;
comp_op = "==" | "!=" | "<" | "<=" | ">" | ">=" ;

(* The key: every gadget MUST end with a return that pops next address *)
return_stmt = "ret" , [ expression ] ;  (* optional value to push before returning *)

(* Expressions *)
expression = literal
           | identifier
           | memory_ref
           | stack_ref
           | binary_expr
           | unary_expr
           | call
           ;

memory_ref = "[" , expression , "]" ;
stack_ref = "stack" , "[" , expression , "]" ;

binary_expr = expression , ( "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" ) , expression ;

unary_expr = "!" , expression ;

(* Initial stack setup - this determines execution order *)
stack_init = "stack" , ":" , "[" , { identifier } , "]" ;

(* Literals and identifiers *)
literal = int_literal | string_literal ;
identifier = letter , { letter | digit | "_" } ;
int_literal = [ "-" ] , digit , { digit } ;
string_literal = '"' , { ? any character except " ? } , '"' ;

letter = "a" | ... | "z" | "A" | ... | "Z" ;
digit = "0" | ... | "9" ;

comment = "//" , { ? any char except newline ? } ;
```

## License

See [LICENSE](LICENSE).

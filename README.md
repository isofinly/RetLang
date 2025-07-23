# Return Oriented Programming Language

Language inspired by ROP concept with mix of assembly syntax and C headers.

It supports linking with external C libraries by either specifying the library name or providing library path to CLI.

Due to simplistic nature of the language, support for imports and external gadgets is not planned.

Currently address space size is limited by constant defined at compiled time.

## Targets

- Any target that is supported by C
- WASM (needs testing)

## Usage

### 1. Compile retc

```bash
cargo build --release
```

### 2. Place retc to your $PATH and then you can use it to compile your ROP programs.

```bash
$ retc -h
RET-lang toolchain

Usage: retc <COMMAND>

Commands:
  lex        [aliases: lx]
  compile    [aliases: c]
  parse      [aliases: p]
  transpile  [aliases: t]
  help       Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

### 3. Create simple program

```ret
external: <stdio.h>

gadget main:
    printf("%s", "Goodbye!")
ret

stack: [main]
```

### 4. Compilation

#### 4.1 Direct compilation

```bash
$ retc c program.ret
```

or specify output file

```bash
$ retc c program.ret -o program
```

or link with library

```bash
$ retc c program.ret -l z
```

Inspecting the binary:

```bash
$ otool -L .build/main
.build/main:
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1351.0.0)

```

#### 4.2 Compilation with external C compiler

```bash
$ retc t program.ret .build/custom.c
```

then run your compiler

```bash
$ tcc -Wall -O3 ./.build/custom.c -o ./.build/main
```

### 5. Debugging IR

You can view lexer, parser outputs by using the `lex` or `parse` commands.

## Grammar

```ebnf
header_name = identifier , { "." , identifier } ;

external = "external" , ":" ,
                 ( "<" , header_name , ">"
                 | '"' , header_name , '"' ) ;

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

assignment = identifier , "=" , expression ;

stack_op = "push" , expression
         | "pop" , identifier
         | "peek" , identifier , "[" , expression "]"  (* peek at stack offset *)
         | "stack_swap" , expression , expression      (* swap stack positions *)
         ;

arithmetic = identifier , arith_op , "=" , expression , arith_op , expression ;
arith_op = "+" | "-" | "*" | "/" | "%" | "&" | "|" | "^" | "<<" | ">>" ;

memory_op = "store" , expression , expression  (* store value at address *)
          | "load" , identifier , expression   (* load from address *)
          ;

conditional_mod = "if" , condition , "then" , identifier , "=" , expression ;

condition = expression , comp_op , expression ;
comp_op = "==" | "!=" | "<" | "<=" | ">" | ">=" ;

(* every gadget MUST end with a return that pops next address *)
return_stmt = "ret" , [ expression ] ;  (* optional value to push before returning *)

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

call = identifier , "(" , [ expression , { "," , expression } ] , ")" ;

(* Determines entrypoint *)
stack_init = "stack" , ":" , "[" , { identifier } , "]" ;

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

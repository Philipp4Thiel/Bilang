# Bilang
aka Pherblang 2.0, this time for real

## Lets plan ahead this time:
1. Parsing `.pherb` files into a AST
2. do typechecking on that AST
3. evaluate that AST
4. might compile that AST to bytecode

## Parsing `.pherb` files into a AST
- [x] parsing with pest.rs
- [ ] get rid of unneeded brackets by rewriting the grammar without any left-recursion
- [ ] parse output of pest into an AST

## Grammar of PherbLang
(this is not the current grammar because i'm too lazy to update everytime i update the pest file, but it's close enough i guess)
```
program = { statement }
 
statement = declaration | printStatement ";"

declaration = "let" IDENTIFIER ":" type "=" expression
printStatement = "print" expression

expression = literal | unary | binary | grouping | IDENTIFIER 
                | lambda | application | "(" expression ")" | conditional

literal = NUMBER
unary = ("-" | "!") expression
binary = expression operator expression
grouping = "(" expression ")"
lambda = ("\" IDENTIFIER { "," IDENTIFIER } "->" expression)
application = expression expression { expression }
conditional = expression "?" expression ":" expression

operator = "+" | "-" | "*" | "/" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "&&" | "||"

type = "int" | type "->" type | "(" type ")"
```

example would be:
```
let x: int = 5;
let y: int = 10;
let add: int -> int -> int = (\a, b -> a + b);
print add x y;

let add5: int -> int = add 5;
print add5 10;

let fib: int -> int = (\n -> n < 2 ? n : fib (n - 1) + fib (n - 2));
print fib 10;

let app: (int -> int) -> int = (\f -> f 5);
print app (\x -> x + 5);
```

## Do typechecking on that AST
- [ ] check if there are any type errors
- [ ] goal for later: infer the types

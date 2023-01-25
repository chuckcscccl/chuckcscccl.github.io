## Chapter 2: Advanced Calculator

In the second chapter of this tutorial, we write a more advanced
version of the calculator example and describe a more complete set of
features as well further details of capabilities introduced in
[Chapter 1][chap1].

The calculator of this chapter supports expressions of the form
**`let x = 1 in (let x = 10 in x*x) + x`** (which should evaluate to 101).
The lexical analyzer and parser must recognize alphanumeric symbols
such as `x` as variables.  This version of the calculator also recongizes
a series of expressions separated by ; (semicolon), giving us the opportunity
to demonstrate a simple error recovery mechanism.

The grammar (with some additions) is as follows:
```
auto
lifetime 'lt
terminals + - * / = ;
terminals let in
lexterminal LPAREN (
lexterminal RPAREN )
valueterminal int ~ i64 ~ Num(n) ~ n
valueterminal var ~ &'lt str ~ Alphanum(n) ~ n
lexattribute set_line_comment("#")

nonterminals Expr ExprList
nonterminal UnaryExpr : Expr
nonterminal LetExpr : Expr

topsym ExprList
resync ;

left * 500
left / 500
left + 400
left - 400

UnaryExpr:Val --> int
UnaryExpr:Var --> var
UnaryExpr:Neg --> - UnaryExpr
UnaryExpr --> LPAREN LetExpr RPAREN

Expr --> UnaryExpr
Expr:Plus --> Expr + Expr
Expr:Minus --> Expr - Expr
Expr:Div --> Expr / Expr
Expr:Times --> Expr * Expr

LetExpr --> Expr
LetExpr:Let --> let var:[let_var] = Expr:init_value in LetExpr:let_body

ExprList:nil -->
ExprList:cons --> LetExpr:car ; ExprList:cdr
```

The grammar takes full advantage of the 'auto' mode: only the types of
values carried by terminal symbols `var` and `int` need to be declared.
All other types and semantic actions are automatically created, as is the
lexical scanner

### Input Lifetime

Rustlr's built-in lexical analyzer, [StrTokenizer<'t>][1] returns
alphanumeric [tokens][rtk] in the form `Alphanum(s)` where `s` is of type
`&'t str`.  The **`lifetime 'lt`** declaration allows type
specifications in the grammar (in `valueterminal` lines) to be
consistent with the generated AST types.  The lifetime is that of the
input.  Currently, only a single lifetime declaration is allowed.
The keywords "let" and "in", though alphanumeric are not recognized as `var`
because they're declared as terminal symbols.


### Additional Tokenizer Customization

The **lexattribute** directive can be used to set any attribute on the
lexer to be generated.  Consult the docs for [StrTokenizer][1]. 
The following samples are valid lexattribute declarations

>      lexattribute keep_newline = true
>      lexattribute keep_comment = true
>      lexattribute keep_whitespace = true
>      lexattribute set_multiline_comments("/* */")
>      lexattribute set_line_comment("")

Setting the line_comment or multiline_comments to the empty string will mean
that such comments are not recognized.  The keep_flags are all false be
default.  **[StrTokenizer][1] recognizes C-style comments by default**.

The presence of these directives automatically enables the -genlex option.
The lexer created is called calc4lexer and is found in with the generated
parser.


### Error Recovery

The language defined by this grammar allows a sequence of arithmetic
expressions to be evaluated in turn by separating them with
semicolons, such as in `2+3; 4-1;`.  The semicolon also allows us to
define a simple error-recovery point: **`resync ;`** indicates that
when a parser error is encountered, the parser will skip past the next
semicolon, then look down its parse stack for a state with which it
can continue parsing.  In otherwords, failure to parse one expression
does not mean it will not try to parse the next ones.  Rustlr does
implement the more traditional LR recovery technique of a designed
*error recovery symbol*, which is demonstrated in a [later
chapter](https://chuckcscccl.github.io/rustlr_project/errors.html).

### Rules of Conflict Resolution

This grammar tempers the use of operator precedence and associativity
declarations with a more formal approach.  Only the precedence of the
binary arithmetic operators are declared.  The precedence of other
expressions, including unary operations, are defined using
different syntactic categories in the form of extra non-terminals
`UnaryExpr` and `LetExpr`.  

The operator precedence and associativity declarations are used to
(statically) resolve *shift-reduce* conflicts in generating the LR
state machine.  A terminal symbol that's to be used as an operator can
be declared as left, right or non-associative and a positive integer
defines the precedence level.  Production rules can also be assigned
precedence as illustrated in [Chapter 1].  If a production rule is not
explicitly assigned a precedence, it is assigned to be the same as
that of the right-hand side symbol with the highest precedence.  The
default precedence of all grammar symbols is zero.  *(Internally, the
precedence is represented by the absolute value of a signed integer:
positive for left-associative, negative for right-associative.  The
second-most significant bit is used to distinguish these from
non-associative values. The default value of zero means that no
precedence is assigned)*.

Rustlr resolves **shift-reduce** conflicts as follows:

  - A lookahead symbol with strictly higher precedence than the rule results
      in *shift*. A warning is always given if the rule has precedence zero.
  - A lookahead symbol with strictly lower precedence than the rule results
      in *reduce*. A warning is always given if the lookahead has precedence zero (undeclared precedence)  
  - A lookahead symbol with the same precedence and associativity as the rule,
      and which is declared right-associative, will result in *shift*.
  - A lookahead symbol with the same precedence and associativity as the rule,
      and which is declared left-associative, will result in *reduce*.
  - In other situations the conflict is *resolved in favor of shift*, with a
      warning sent to stdout regardless of trace level.  All shift-reduce
      conflicts are warned at trace level 5 or higher.

Rustlr also resolves **reduce-reduce** conflicts by always
favoring the rule that appears first in the grammar, although a
warning is always sent to stdout regardless of trace level.

With these rules of resolution, rustlr will generate some
deterministic parser for any grammar.  But grammars with unresolved
conflicts usually indicate that the grammar contains serious problems
that, if left unattended, will eventually lead to unexpected
consequences.  Fixing the conflicts is often non-trivial, which is
part of the learning curve of LR parsing.  A parser generator that
does not warn of conflicts is like a compiler that does not give any
warnings or error messages.



### Rules of AST Generation

The three principal types of expressions, `Expr`, `LetExpr` and
`UnaryExpr` define three levels of precedence. From weakest to
strongest: `LetExpr`, `Expr`, and `UnaryExpr`.  Generally speaking, a
new type is created for each non-terminal symbol of the grammar, **which
will also share the same name as the non-terminal itself**.  However, in
this grammar we specify that 'LetExpr' and 'UnaryExpr' are
also expressions at the AST level, and should be considered additional
variants of 'Expr':
```
nonterminal UnaryExpr : Expr
nonterminal LetExpr : Expr
```
The ASTs derived from the productions for `UnaryExpr` and `LetExpr`
*extend* the enum that's created for `Expr`.  The type created
for Expr must be an enum for this to work (it would not work if it was
a struct).  The ASTs generated for the grammar of this chapter are
```
#[derive(Debug)]
pub enum Expr<'lt> {
  Plus(LBox<Expr<'lt>>,LBox<Expr<'lt>>),
  Times(LBox<Expr<'lt>>,LBox<Expr<'lt>>),
  Div(LBox<Expr<'lt>>,LBox<Expr<'lt>>),
  Minus(LBox<Expr<'lt>>,LBox<Expr<'lt>>),
  Neg(LBox<Expr<'lt>>),
  Val(i64),
  Var(&'lt str),
  Let{let_var:&'lt str,init_value:LBox<Expr<'lt>>,let_body:LBox<Expr<'lt>>},
  Expr_Nothing,
}
impl<'lt> Default for Expr<'lt> { fn default()->Self { Expr::Expr_Nothing } }

#[derive(Debug)]
pub enum ExprList<'lt> {
  nil,
  cons{car:Expr<'lt>,cdr:LBox<ExprList<'lt>>},
  ExprList_Nothing,
}
impl<'lt> Default for ExprList<'lt> { fn default()->Self { ExprList::ExprList_Nothing } }
```

An enum is created for each non-terminal symbol of the grammar that
appears on the left-hand side of multiple production rules, unless the
type of the non-terminal is declared to "extend" another type as
explained above. The name of the enum is the same as the name of the
non-terminal.  The names of the variants are derived from the labels
given to the left-hand side nonterminal, or are automatically
generated from the nonterminal name and the rule number (e.g. `Expr_8`).
A special `Nothing` variant is also created to represent a default.
There is normally an enum variant for each production rule of this
non-terminal.  Each variant is composed of the right-hand side symbols
of the rule that are associated with *non-unit* types.  If none of the
right-hand side symbols are given labels, a tuple-variant is created.  The
presence of any right-hand side label will result in a struct-like variant
with named fields: the names will correspond to the labels, or are 
generated automatically in the form `_item{i}_` where `i` refers to
the position of the symbol on the right-hand side.
Unit-typed values can also become part of the enum if the symbol is given an
explicit label.  For example: **` A:case1 --> a B `** where terminal symbol `a`
is of unit type, will result in a enum variant
`case1(B)`. whereas **` A:acase --> a:m B `** will result in a
variant `case1{m:(), _item1_:B}`.  It is recommended that either
labels are given to all right-hand side symbols that are to be included in
the variant, or to none at all.

A struct is created for any non-terminal symbol that appears on the
left-hand side of exactly one production rule, unless the type of that
nonterminal is declared to extend another type.
You can also force an enum to be created instead of a struct by
giving the singleton rule a left-hand side label, in which case the label
will name the lone variant of the enum (besides the `_Nothing` default).
This would be required when you know that the type will be extended with
other variants, as demonstrated above.

The struct may be empty if all right-hand-side symbols of the single production
rule are associated with the unit type and do not have labels.  In such
cases, the *flatten* directive can eliminate the presence of these structs
from ASTs (see below).

Rustlr will generate code to derive or implement the Debug and Default
traits for all structs (this works fine for recursive structs).

The name of the struct is the same as the non-terminal.  If any of the grammar symbols
on the right-hand side of the rule is given a label, it would create a struct
with the fields of each struct named by these labels, or
with `_item{i}_` if
no labels are given.  For example, a nonterminal `Ifelse` with a singleton rule
  ```
  Ifelse --> if Expr:condition Expr:truecase else Expr:falsecase
  ```
will result in the generation of:
  ```
  #[derive(Default,Debug)]
  pub struct Ifelse {
    pub condition: LBox<Expr>,
    pub truecase: LBox<Expr>,
    pub falsecase: LBox<Expr>,
  }
  ```
The presence of [LBox][2] assumes that `Ifelse` is mutually recursive with
`Expr`, which it likely is in such grammars.
If none of the symbols on the right have labels, rustlr creates a tuple
struct.  For Example a singleton rule such as **`whileloop --> while ( expr ) expr`**
will produce an a `struct whileloop(expr,expr);`  Be careful to avoid
using Rust keywords as the names of non-terminals.

Rustlr calculates a reachability closure so it is aware of which
non-terminals are mutually recursive.  It uses this information to
determine where smart pointers are required when defining these
recursive types.  Rustlr always uses its [LBox][2] custom smartpointer
to also include line/column information.  Notice that the variant
`enum::cons` has only the second component in an LBox.  One can, for
the sake of recording position information, always create an LBox
regardless of reachability by giving the component a "boxed label".
That is,
```
  ExprList:cons --> Expr:[car] SEMICOLON ExprList:cdr
```
will generate a variant that also has its first component in an
LBox. The 'cdr' is already an LBox as required for recursion (writing
`[cdr]` will have no additional effect).  The reachability relation also
determines if a type requires a lifetime parameter.

Although the generated parser code may not be very readable, rustlr also generated semantic actions that create instances of these AST types.  For example, the rule `Expr:Plus --> Expr + Expr` will have a semantic action equivalent
the following, manually written one:
```
Expr --> Expr:[a] + Expr:[b] {Plus(a,b)}
```
When manually writing semantic actions, a label of the form `[a]` indicates
to the parser to place the semantic value associated with the symbol in
an [LBox][2].


#### **'Passthru'**

There are three production rules in the grammar that do not
correspond to enum variants: `Expr --> UnaryExpr`, `LetExpr --> Expr`
and `UnaryExpr --> LPAREN LetExpr RPAREN`. 
Rustlr infers from the fact that
  1. there is no left-hand side label for any of these rules
  2. There is exactly one grammar symbol on the right-hand side that has a non-unit
     type, and that type is the same as the type of the left-hand side symbol.
     The other symbols, if any, are of unit type
  3. There are no labels nor operator precedence/associativity declarations for the other symbols.
     
For the rule `UnaryExpr --> LPAREN LetExpr RPAREN`, it therefore infers that
the parentheses on the right hand side carry no meaning at the AST level, and
thus generates a semantic action for this rule
that would be equivalent to:
```
  UnaryExpr --> LPAREN LetExpr:e RPAREN { e }
```
We refer to such cases as "pass-thru" cases.  If the automatically
inferred "meaning" of this rule is not what's desired, it can be
altered by using an explicit left-side label: this will generate a
separate enum variant (at the cost of an extra LBox) that
distinguishes the presence of the parentheses.  Note that the 
rule `UnaryExpr:Neg --> - UnaryExpr`, was not recognized as a pass-thru
case by virtue of the left-hand side label `Neg`.  Unlike the parentheses,
the minus symbol certain has meaning beyond the syntactic level.
We can also force the minus sign to be
included in the AST by giving it an explicit lable such as `-:minus UnaryExpr`.
This would create an enum variant that includes a unit type value.


#### Flattening Structs

Rustlr provides another way to control the generation of ASTs so that
it is not always dependent on the structure of the grammar, although
it is not illustrated in the calculator example.  When writing a
grammar, we sometimes create extra non-terminal symbols and rules for the
purpose of organization.  As an abstract example:
```
A --> a Threebs c
Threebs --> b b b
```
Rustlr will create two tuple structs for these types. Assuming that a, b, c
are not of unit type, there will be a `struct A(a,Threebs,c)` and a
`struct Threebs(b,b,b)`.  However, it is possible to declare in the grammar,
once the non-terminals `A` and `Threebs` have been declared, that the
type `Threebs` can be **flattened** into other structures:
```
flatten Threebs
```
This means that the AST for Threebs should be absorbed into other types if
possible (multiple nonterminals can be so declared on the same line).
This will still create a `struct Threebs(b,b,b)`, but it will create for A:
**`struct A(a,b,b,b,c)`**.

Both structs and enums can absorb 'flatten' types.  However, there
are several enforced rules governing the flattening of types:
  1. Only struct types can be flattened: thus only nonterminals that has but a
  single production rule can have its AST absorbed into other types. Enum
  types can absorb 'flatten' structs but cannot be absorbed into other types.
  2. Types already defined to 'extend' the enum of another type cannot be
  flattened
  3. A tuple struct can only absorb the flattened form of another tuple struct.
  In the above example, if `Threeb` was a non-tuple struct with named fields (which can be created
  by giving of the the b's a label), then it cannot be absorted into `A`.
  4. A boxed-labeled field cannot absorb a 'flatten' type.  That is, if
  the rule for `A` above was written `A --> a:a Threebs:[b] c:c` then the AST
  for A would become `pub struct A{a:a, b:LBox<Threebs>, c:c}`.  This is
  the only way to prevent the absorption of a 'flatten' type on a case-by-case
  basis.
  5. Mutually recursive types cannot flatten into each other.
  6. Nested flattening is not currently supported.  This is a temporary restriction.

Point 5 is rather subtle.  Consider productions rules `A --> B` and
`B --> A`.  It is perfectly valid to declare `flatten B`: This will
result in a `struct A(LBox<A>)`: the [LBox][2] is created for the AST of B using reachability calculations.  What we cannot have is `flatten A` and 
`flatten B`: the flattening is only allowed in one direction.  Otherwise we
would be replacing B with A and A with ... what?  One consequence of
this restriction is that a type cannot flatten into itself: `B --> B`
would not be valid for `flatten B`: *B is mutually recursive with
itself.*

The last restriction is related to the mutual-flattening restriction.  However,
there are cases where it would be safe to flatten A into B and then flatten
B into C. This ability is not currently supported (as of Rustlr 0.3.5).



#### Emphasizing the Importance of Labels

The usage of labels greatly affect how the AST datatype is
generated.  Labels on the left-hand side of a production rule give
names to enum variants.  Their presence also cancel "pass-thru"
recognition by always generating an enum variant for the rule.
A left-hand side label will also prevent a struct from being generated even
when a nonterminal has but a single production rule.
The absence of labels on the right-hand side leads to the creation of
tuple variants or structs.  The presence of right-side labels creates
structs or struct-variants with named fields.
A label on unit-typed grammar symbol means that the symbol won't be
ignored and will be included in the the type.  If a non-terminal has a
single production rule, the lack of any labels left or right leads
to the creation of a simpler tuple struct.  The use of boxed
labels such as `[e]` forces the semantic value to be wrapped inside an LBox
whether or not it is required to define recursive types.  Boxed labels also
prevent the absorption of 'flatten' types.


#### Overriding Types and Actions

It is always possible to override the automatically generated types and actions.
In case of ExprList, the labels 'nil' and 'cons' are sufficient for rustlr to create a linked-list data structure.  However, the right-recursive grammar rule is slightly non-optimal for LR parsing (the parse stack grows until the last element of the list before ExprList-reductions take place).  One might wish to use a left-recursive rule and a Rust vector to represent a sequence of expressions.  This can be done in several ways, one of which is by making the following changes to the grammar.  First, change the declaration of the non-terminal symbol `ExprList` as follows:

```
nonterminal ExprList Vec<LBox<Expr<'lt>>>
```
You probably want to use an LBox even inside a Vec to record the
line/column position information.  When writing your own types for
non-terminals, it's best to examine the types that are generated to
determine their correct usage: for example, whether a lifetime
parameter is required for `Expr`.  It is also possible to write the above as
```
nonterminal ExprList Vec<LBox<@Expr>>
```
The symbol `@` indicates that the type of LBox will be determined by whatever
is the type associated with the nonterminal `Expr`.

Now replace the two production rules for `ExprList` with the following:

```rust
ExprList --> { vec![] }
ExprList --> ExprList:ev LetExpr:[e] ; { ev.push(e); ev }
```
The presence of a non-empty semantic action will override automatic AST generation. It is also possible to **inject custom code into the
automatically generated code**:
```
ExprList -->  {println!("starting a new ExprList sequence"); ... }
```
The ellipsis are allowed only before the closing right-brace.  This indicates
that the automatically generated portion of the semantic action should follow.
The ellipsis cannot appear anywhere else.


An easier way to parse a sequence of expressions separated by ; and to
create a vector for it, is to
use the special suffixes `+`, `*`, `?`, `<_*>` and `<_+>`.
These are described in [next chapter][chap5].



### Creating and Invoking the Parser

To build a working parser and evaluator, `cargo new` a crate with "rustlr = 0.4"
in its dependencies. Copy the grammar into the crate directory as
'calcauto.grammar'.  Then run

>  **`rustlr calcauto.grammar -o src/`**.

#### **rustlr** command line options

The first and the only required argument to the executable is the path of the
grammar file.  Optional arguments (after the grammar path) that can be
given to the executable are:

- **-lr1** : this will create a full LR(1) parser if LALR does not suffice.
  The default is LALR, which works for most examples.  A sample grammar
  requiring full LR(1) can be found **[here](https://cs.hofstra.edu/~cscccl/rustlr_project/nonlalr.grammar).**
  Rustlr will always try to resolve shift-reduce conflicts by precedence and associativity
  declarations (see later examples) and reduce-reduce conflicts by rule order.
  So it will generate some kind of parser in any case.  The next chapter will
  explain in detail how conflicts are resolved.
- **-o filepath** : changes the default destination of the generated parser
  and AST files.
- **-nolex** : skips the automatic generation of a lexical scanner using the
built-in [StrTokenizer][1].  This option is not recommended
- **-auto** or **-genabsyn** : automatically generates abstract syntax data types and required semantic actions.  See [Chapter 4][oldchap4].  This feature is not recommended for beginners.
- **-trace n**  : where n is a non-negative integer defining the trace level.
  Level 0 prints nothing; level 1, which is the default, prints a little more
  information.  Each greater level will print all information in lower levels.
  -trace 3 will print the states of the LR finite state machine, which could
  be useful for debugging and training the parser for error message output.
- **-nozc** : this produces an older version of the runtime parser that does not use
  the new zero-copy lexical analyzer trait.  This option is only retained
  for backwards compatibility with grammars and lexical scanners written prior
  to rustlr version 0.2.0.  This option is not capable of generating a lexical
  scanner.
- **-lrsd** : enables "LR parsing with selective delays".  This is an
  experimental (but usable) extention of LR(1) parsing and accepts a larger
  class of grammars.  See the [Appendix][appendix] for details.


#### Sample main

Assuming that `calcautoparser.rs` and `calcauto_ast.rs` are in the crate's
`src/` directory.  Copy the sample input file , [input.txt](https://github.com/chuckcscccl/rustlr/blob/main/examples/autocalc/input.txt)
into the crate directory.
The input intentionally contains both syntactic and semantic errors to
test error reporting and recovery.
Copy [main.rs](https://github.com/chuckcscccl/rustlr/blob/main/examples/autocalc/src/main.rs) into `src/`.
The `main` function in this main.rs invokes the parser and evaluates the
cons-list of expressions returned.
```
fn main() {
   let src = rustlr::LexSource::new("input.txt").expect("input not found");
   let mut scanner4 = calcautoparser::calcautolexer::from_source(&src);
   let mut parser4 = calcautoparser::make_parser();
   let tree4= calcautoparser::parse_with(&mut parser4, &mut scanner4);
   let result4 = tree4.unwrap_or_else(|x|{println!("Parsing errors encountered; 
results are partial.."); x});
   println!("\nABSYN: {:?}\n",&result4);
   let bindings4 = newenv();
   println!("\nresult after eval: {:?}", eval_seq(&bindings4,&result4,1));
}//main   
```
The last two lines of main relies on additional structures and functions
defined inside [main.rs](https://github.com/chuckcscccl/rustlr/blob/main/examples/autocalc/src/main.rs).  



---------------

### Training The Parser For Better Error Reporting

It is recommended that, when a parser is generated, the -trace 3 option is
given, which will print all the LR states that are created. This may be helpful
when training the parser.  Each time the parser is regenerated the states may
have different numbers identifying them, even if the grammar is unchanged.

With a newly generated parser, when a parser error is encountered, the
line and column numbers are printed and an "unexpected symbol" error
message is given. To print more helpful error messages, the parser can
be trained interactively.  Interactive training also produces a script
for future, automatic retraining when a new parser is generated.

Modify [main.rs](https://cs.hofstra.edu/~cscccl/rustlr_project/calc4/src/main.rs) by uncommenting lines 2 and 3 in the input:
```
3(1+2)   # syntax (parsing) error
5%2;   # syntax error
```
Note that the supplied main already calls `parse_train(&mut scanner2,"calc4parser.rs");`  For input with no errors, this call works the same way as `parse(&mut scanner2);`  The [parse_train](https://docs.rs/rustlr/latest/rustlr/runtime_parser/struct.RuntimeParser.html#method.parse_stdio_train) function takes a path to a copy of the parser being trained (it's not recommended to change the copy that
you're using this way).
Cargo run will lead to the following (possible) training session, depending on
user input:
```
PARSER ERROR: unexpected symbol ( on line 2, column 2 ..

>>>TRAINER: if this message is not adequate (for state 1), enter a replacement (default n
o change): missing an operator symbol such as *
>>>TRAINER: should this message be given for all unexpected symbols in the current state?
 (default yes) no
PARSER ERROR: unexpected symbol % on line 3, column 2 ..

>>>TRAINER: if this message is not adequate (for state 1), enter a replacement (default n
o change): this symbol is not recognized as a valid operator in this language
Expression tree from parse: Seq([Minus(Negative(Val(5)), Times(Minus(Val(4), Val(2)), Val
(5))), Minus(Minus(Val(5), Val(7)), Negative(Val(9))), Minus(Times(Val(4), Val(3)), Val(9
)), Plus(Val(2), Divide(Val(1), Minus(Minus(Val(2), Val(1)), Val(1)))), Letexp("x", Val(1
0), Plus(Val(2), Var("x"))), Letexp("x", Val(1), Plus(Plus(Var("x"), Letexp("x", Val(10),
 Plus(Var("x"), Var("x")))), Var("x"))), Plus(Letexp("x", Val(2), Plus(Var("x"), Var("x")
)), Var("x")), Plus(Letexp("x", Val(4), Divide(Var("x"), Val(2))), Letexp("x", Val(10), T
imes(Var("x"), Letexp("y", Val(100), Divide(Var("y"), Var("x"))))))])
---------------------------------------

result for line 1: -15 ;
result for line 4: 7 ;
result for line 5: 3 ;
Division by zero (expression starting at column 5) on line 6 of Val(1) at column 3 ... Er
ror evaluating line 6;
result for line 7: 12 ;
result for line 8: 22 ;
UNBOUND VARIABLE x ... Error evaluating line 9;
result for line 10: 102 ;
Parser error, best effort after recovery: Some(102)
```
Notice that error recovery was effective and the parser still produced a usable
parse tree: however, the parser's error_occurred flag will be set.  It is
under consideration as to whether future editions of Rustlr will also allow the
error-recovery strategy to be trainable in the same way.  For now, only a fixed
number of strategies are available.  In the opinion of the author, the resync
technique is the simplest and most effective.

If the augmented parser is used on the same input, it will display the trained
message in addition to "unexpected symbol..."

You can see how training augments the LR state transition table by
examining the `load_extras` function at the end of the generated parser:
```
fn load_extras(parser:&mut RuntimeParser<Expr,Expr>)
{
  parser.RSM[1].insert("(",Stateaction::Error("missing an operator symbol such as *"));
  parser.RSM[1].insert("ANY_ERROR",Stateaction::Error("this symbol is not recognized as a
 valid operator in this language"));
}//end of load_extras: don't change this line as it affects augmentation
```
When the "unexpected symbol" is recognized as a declared symbol of the grammar, the trainer will be given the option of entering the error message for either
just that symbol, or all unexpected symbols in the same state.  If the latter is
chosen then an entry is created for the reserved `ANY_ERROR` symbol.  If the
unexpected symbol is not recognized as a terminal symbol of the grammar, an
`ANY_ERROR` entry is always created.  You can see the contents of "state 1"
if you created it with the -trace 3 option. You will of course have to understand the LR parsing algorithm to make use of the information.

When the modified parser runs and encounters another unexpected symbol in the
same state, it will first see if there is an entry for that symbol; if none
exists, it will look for an `ANY_ERROR` entry for a message to display.
Thus the two entries do not conflict with eachother.

The interactive session also generated a script file, which would be called
*"calc4parser.rs_script.txt"*, with the following contents:
```
# Rustlr training script for calc4parser.rs

2       2       ( ::: missing an operator symbol such as *
3       2       ANY_ERROR ::: this symbol is not recognized as an operator in this language
```
This script can be used to retrain a newly genenerated parser (with different state numbers) with the [train_from_script](https://docs.rs/rustlr/latest/rustlr/runtime_parser/struct.RuntimeParser.html#method.train_from_script) function
provided the same input from the original training.  The line and column numbers
of where the errors are expected are recorded in the script.  Please note that
training from script has not yet been tested on a large scale.  

------------

[1]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.StrTokenizer.html
[2]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LBox.html
[3]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LRc.html
[4]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.ZCParser.html#method.lbx
[5]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.StackedItem.html#method.lbox
[sitem]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.StackedItem.html
[chap1]:https://chuckcscccl.github.io/rustlr_project/chapter1.html
[chap3]:https://cs.hofstra.edu/~cscccl/rustlr_project/chapter3.html
[chap4]:https://cs.hofstra.edu/~cscccl/rustlr_project/chapter4.html
[chap5]:https://chuckcscccl.github.io/rustlr_project/chapter5.html
[lexsource]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.LexSource.html
[drs]:https://docs.rs/rustlr/latest/rustlr/index.html
[tktrait]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/trait.Tokenizer.html
[tt]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html
[rtk]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/enum.RawToken.html
[fromraw]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.from_raw
[nextsymfun]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/trait.Tokenizer.html#tymethod.nextsym
[zcp]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.ZCParser.html
[ttnew]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.new

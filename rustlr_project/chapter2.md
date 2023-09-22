## Chapter 2: Advanced Examples

In the second chapter of this tutorial, we describe a more complete set of
features of rustlr as well further details of it's capabilities introduced in
[Chapter 1][chap1].

The language defined by the first grammar of this chapter supports expressions
of the form
**`let x = 1 in (let x = 10 in x*x) + x`** (which should evaluate to 101).
The lexical analyzer and parser must recognize alphanumeric symbols
such as `x` as variables.  This version of the calculator also recongizes
a series of expressions separated by ; (semicolon), giving us the opportunity
to demonstrate a simple error recovery mechanism.

The grammar is as follows:
```
# Advanced Calculator
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
LetExpr:Let --> let var:let_var = Expr:init_value in LetExpr:let_body

ExprList:nil -->
ExprList:cons --> LetExpr:car ; ExprList:cdr
# alternative, will create a vector:
# ExprList --> (LetExpr ;)*
```

The grammar takes full advantage of the 'auto' mode: only the types of
values carried by terminal symbols `var` and `int` need to be declared.
All other types and semantic actions are automatically created, as is the
lexical scanner

### 1. Creating and Invoking the Parser

To build a working parser and evaluator, `cargo new` a crate with "rustlr = 0.4"
in its dependencies. Copy the grammar into the crate directory as
**'calcauto.grammar'**.  Then run

>  **`rustlr calcauto.grammar -o src/`**.

#### **Rustlr Command-Line Options**

The first and the only required argument to the executable is the path of the
grammar file.  Optional arguments (after the grammar path) that can be
given to the executable are:

- **-lr1** : this will create a full LR(1) parser if LALR does not suffice.
  The default is LALR, which works for most examples.  A sample grammar
  requiring full LR(1) can be found **[here](https://cs.hofstra.edu/~cscccl/rustlr_project/nonlalr.grammar).**
- **-o filepath** : changes the default destination of the generated parser
  and AST files.  The filepath can be a directory ending in `\` or `/`, or a
  file path for the parser: it will also save the AST file in the same
  directory.
- **-nolex** : skips the automatic generation of a lexical scanner using the
built-in [StrTokenizer][1].  This option is not recommended.
- **-auto** : automatically generates abstract syntax.  This is also enabled
  if the grammar begins with the `auto` directive.
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

From `calcauto.grammar` rustlr generates `calcautoparser.rs` and `calcauto_ast.rs` in the crate's
`src/` directory.  Copy the sample input file, [input.txt](https://github.com/chuckcscccl/rustlr/blob/main/examples/autocalc/input.txt)
into the crate directory.
The input intentionally contains both syntactic and semantic errors to
test error reporting and recovery.
Copy [main.rs](https://github.com/chuckcscccl/rustlr/blob/main/examples/autocalc/src/main.rs) into `src/`.
The `main` function in this main.rs invokes the parser and evaluates the
list of expressions returned.
```
fn main() {
   let src = rustlr::LexSource::new("input.txt").expect("input not found");
   let mut scanner4 = calcautoparser::calcautolexer::from_source(&src);
   let mut parser4 = calcautoparser::make_parser();
   let tree4= calcautoparser::parse_with(&mut parser4, &mut scanner4);
   let result4 = tree4.unwrap_or_else(|x|{println!("Parsing errors encountered; 
results are partial.."); x});
   println!("\nABSYN: {:?}\n",&result4);
   let bindings = newenv();
   println!("\nresult after eval: {:?}", eval_seq(&bindings,&result4,1));
}//main   
```
The last two lines of main relies on additional structures and functions
defined inside [main.rs](https://github.com/chuckcscccl/rustlr/blob/main/examples/autocalc/src/main.rs).  

The crate is now ready for **`cargo run`**. You will see that error recovery
was effective and results were displayed for lines that parsed correctly.

--------------

We now examine more closely the different components of the grammar.


### 2. Input Lifetime

Rustlr's built-in lexical analyzer, [StrTokenizer<'t>][1] returns
[tokens][rtk] such as `Alphanum(s)` where `s` is of type
`&'t str`.  The **`lifetime 'lt`** declaration allows type
specifications in the grammar (in `valueterminal` lines) to be
consistent with the generated AST types.  The lifetime is that of the
input.  Currently, only a single lifetime declaration is allowed.
The keywords "let" and "in", though alphanumeric are not recognized as `var`
because they're declared as terminal symbols.


### 3. Configuring the Lexical Scanner

A lexical scanner is automatically created from the declarations of terminal
symbols in the grammar.  Terminal symbols that do not carry values (unit-typed)
may be declared verbatim, such as
```
  terminal , ; + - * /
```
or by associating a name with the textual form:
```
  lexterminal COLON :
```
Reserved symbols including `:`, `|`, `%`, `!%`, `{`, `}`, `-->`, `==>`, and `<==`
must be declared this way.

Terminal symbols that carry common types of values can be declared using
**`valterminal`** as explained in the [previous chapter][chap1].
More generally, they can be declared using **`valueterminal`**:

>  *valueterminal terminal_name ~ terminal_type ~ expected_token ~ token_value*

The last two components specify the lexical token that's to be associated
with the terminal and the expression that would extract the semantic value
from the token.  The value must be of the specified type.
The built-in lexical scanner, [StrTokenizer][1], recognizes a number
of common categories of lexical tokens, with the option to add custom
categories.  Users should become familiar with the token type,
**[RawToken<'t>][rtk]**, which consists of the following principal
variants:

 - **Alphanum(&'t str)**: where the string represents an (ascii) alphanumeric
   symbol that does not start with a digit.  The underscore character is
   also recognized as alphanumeric.
 - **Symbol(&'t str)**: a string consisting of non alphanumeric characters such as "==". Longer sequences (up to 3) have priority over shorter prefixes.
 - **Num(i64)**: Both decimal and hexidecimals (starting
 with "0x") are recognized as Nums.  However, although the returned value is signed,
 a negative integer such as "-12" is recognized as a Symbol("-") followed by a Num(12),
 and thus must be recognized at the parser level.  Despite this, it is still more convenient
 to return the more generic signed form.  Also, "3u8" would be
 reconized as a Num(3) followed by an Alphanum("u8").
 - **Float(f64)**: like the case of Num, this represents unsigned, decimal floats.
 - **BigNumber(&'t str)**: Numbers that are too large for i64 or f64 are represented verbatim.
 - **Char(char)**: this represents a character literal in single quotes such as 'c'
 - **Strlit(&'t str)**: A string literal delineated by double quotes.  These strings can span multiple lines and can contain nested, escaped quotes.  **The
 surrounding double quotes are included in the literal**.
 - **Newline**: optional token indicating a newline character. These tokens
 are **not** returned by the tokenizer by default, but can be returned with
 the directive
   > lexattribute keep_newline = true
 - **Whitespace(usize)**: another optional token that carries the number of
   consecutive whitespaces.  This option is likewise enabled with
   > lexattribute keep_whitespace = true   
 - **Verbatim(&'t str)**: another optional token carrying verbatim text, usually
   comments.  Enable with
   > lexattribute keep_comment = true
   
   **By default, [StrTokenizer][1] recognizes C-style comments**, but this can
   be customized with, for example,
   > lexattribute set_line_comment("#")

 - **Custom(&'static str, &'t str)**: user-defined token type.  The static
   string names the token type and the other string 
   points to raw text.
   This token type is intended to be paired with declarations in the
   grammar such as
   
   > lexattribute add_custom("uint32",r"^[0-9]+u32")

   Text matching the given [regular expression][regex] will be returned as a
   **`RawToken::Custom("uint32",_)`** token.  Custom regular expressions
   should not start with whitespaces and will override all other token types.
   Multiple custom types are matched by the order in which they appear in
   the grammar file.  An anchor (^) will always
   be added to the start of the regex if none is given.

The **lexattribute** directive can also set other properties of the tokenizer.
Consult the docs for [StrTokenizer][1] for these properties.  Valid
`lexattribute` directives also include

>      lexattribute tab_spaces = 8
>      lexattribute set_multiline_comments("/* */")
>      lexattribute set_line_comment("")

Setting the line_comment or multiline_comments to the empty string will mean
that such comments are not recognized.  The `keep_` flags are all false by
default.  

**Please note that malformed lexattribute declarations will only be 
reported when the generated parser is compiled.**

There are also mechanisms for reconfiguring the tokenizer at runtime, which
are needed in some occassions.  We shall discuss these in a special chapter.


### 4. Error Recovery

The language defined by this grammar allows a sequence of
expressions to be evaluated in turn by separating them with
semicolons, such as in `2+3; 4-1;`.  The semicolon also allows us to
define a simple error-recovery point: **`resync ;`** indicates that
when a parser error is encountered, the parser will skip past the next
semicolon, then look down its parse stack for a state with which it
can continue parsing.  In otherwords, failure to parse one expression
does not mean it will not try to parse the next ones.  More than one
symbol can be identified as resynch points.  Rustlr also
implement the more traditional LR recovery technique of a designed
*error recovery symbol*, which is demonstrated in a [later
chapter](https://chuckcscccl.github.io/rustlr_project/errors.html).

### 5. Rules of Conflict Resolution

This grammar tempers the use of operator precedence and associativity
declarations with a more formal approach.  Only the precedence of the
binary arithmetic operators are declared.  The precedence of other
expressions, including unary operations, are defined using
different syntactic categories in the form of extra non-terminals
`UnaryExpr` and `LetExpr`.  

The operator precedence and associativity declarations are used to
(statically) resolve *shift-reduce* conflicts in generating the LR
state machine.  A terminal symbol that's to be used as an operator can
be declared as left, right or non-associative (`nonassoc`) and a positive integer
defines the precedence level.  Production rules can also be assigned
precedence as illustrated in [Chapter 1][chap1].  If a production rule is not
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
deterministic parser for any grammar.  But grammars with unexplained
conflicts usually indicate that the grammar contains serious problems
that, if left unattended, will eventually lead to unexpected
consequences.  Fixing the conflicts is often non-trivial, which is
part of the learning curve of LR parsing.  A parser generator that
does not warn of conflicts is like a compiler that does not give any
warnings or error messages.

-------------------

### 6. Rules of AST Generation

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
`case1(B)`. whereas **` A:case1 --> a:m B `** will result in a
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
will produce a `struct whileloop(expr,expr);`  Be careful to avoid
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
`[cdr]` will have no additional effect).

It is also possible to give a symbol an empty box label, which may be
desired if one still wishes to create a tuple struct/variant:
```
 ExprList:cons --> Expr:[] SEMICOLON ExprList
```
will generate the tuple variant `cons(LBox<Expr>,LBox<ExprList>)`

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


#### Variant Groups

Another choice in AST generation is the designation of "variant groups".
This feature makes it possible to write production rules without
any labels while still allowing meaningful AST types to be created.
Instead of a separate enum variant for each production rule, one can unite
several rules under a single variant, discriminated by the name of some
grammar symbol that's designated in the grammar as belonging to a group.
Usually, the grammar symbols are operators.  For example, the grammar
```
auto
valueterminal int ~ i64 ~ Num(n) ~ n
terminals + - ( )
nonterminal E
nonterminal T : E
topsym E

variant-group-for E BinaryOp + -

E --> E + T | E - T | T
T:Neg --> - T
T:Val --> int
T --> ( E )
```
will generate the following enum type for `E`:
```
#[derive(Debug)]
pub enum E {
  BinaryOp(&'static str,LBox<E>,LBox<E>),
  Val(i64),
  Neg(LBox<E>),
  E_Nothing,
}
```
An expression such as `3+4` will be parsed as `E::BinaryOp("+",a,b)`
where `a`, `b` are LBoxes containing `E::Val(3)` and `E::Val(4)`.

The **`variant-group-for`** grammar directive associates the name of a
nonterminal symbol (`E` in this case), and a group of grammar symbols
(terminal or non-terminal) with a the name of a "group", in this case
`BinaryOp`.  All production rules for the designated nonterminal on
the left, with right-hand sides that contain these symbols can then be
united under a single enum variant, discriminated by a static string
that corresponds to the **print name** of the grammar symbol.  
Typically, these symbols will be terminals representing operators. If
the right-hand side of a rule contains multiple symbols that can be
grouped, only the first one (from left to right) will have effect.
Furthermore, *only rules that contain no explicit left- or right-
labels* are subject to such grouping.  In particular, the rule
`T:Neg --> - T` still generates the separate variant for `Neg` because of the
presence of left-hand side label.  The presence of any right-hand side label,
including `[]`, will likewise cancel grouping for that production.
Thus, **a single tuple variant is created for each declared group.**

Note: because the variant group operator is distinguished by a static
string, rustlr will choose the lexical (print) form of the operator.
That is, if you declared `lexterminal Plus +` then the static string
will be `"+"` and the AST will be something of the form
`BinaryOp("+",_,_)`.  However, when declaring the variant group either the
print name or the grammar name may be used.

There is also a deprecated `variant-group` directive that does not
name a specific nonterminal to associate the grouping with.  This form
is not recommended except for small grammars.  But in languages where,
for example, `*` can mean more than multiplication, the
`variant-group-for` directive is more appropriate.


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
prevent the absorption of 'flatten' types.  The absence of labels is
required to enable the `variant-group` feature.


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
Expr:Times -->  Expr * Expr {println!("parsing a times-expression"); ... }
```
The ellipsis are allowed only before the closing right-brace.  This indicates
that the automatically generated portion of the semantic action should follow.
The ellipsis cannot appear anywhere else.


-------------

### 7. Using Regular-Expression Style Operators in Grammars

Rustlr recognizes regular-expression style symbols *, + and ? in
production rules, which lead to the generation of new production rules
and semantic actions that create Vectors and Options (for ?).
However, these symbols cannot be used unrestrictedly to form arbitrary
regular expressions. They cannot be nested (see reason below).

Again referring the advanced calculator grammar,
another way to define the nonterminal `ExprList` as a semicolon-separated
sequence of `LetExpr` is to replace the two productions for `ExprList` with
the following rule
```
ExprList --> (LetExpr ;)*
```
This would lead to the generation of a tuple struct for type ExprList:
```
#[derive(Default,Debug)]
pub struct ExprList<'lt>(pub Vec<LC<Expr<'lt>>>,);
```
Vectors created for these operators always contain values inside **[LC][lc]**
structures.  LC contains an AST expression along with its lexical
line/column information in an open tuple as opposed to 
[LBox][2], which encapsulates a [Box][box]. The Box is not necessary here as Vectors
are already heap-allocated.

to retain the lexical-position information.
The operator **`*`** means a sequence of zero or more.  This is done by generating several new non-terminal symbols internally.
Essentially, these correspond to
```
ES0 --> LetExpr:e ; {e}
ES1 --> { Vec::new() }
ES1 --> ES1:v ES0:[e] { v.push(e); v }
ExprList --> ES1:v {v}
```
These rules replace the original in the grammar.  In the `auto` mode,
rustlr also infers that symbols such as ; has no meaning at the
AST level (because it has the unit type and noprecedence/associativity
declaration). It therefore infers that the type of the nonterminal ES0
is the same as Expr, and automatically generates the appropriate semantic action.
If override of this behavior is required, one can manually rewrite the grammar
as
```
ES0:SEMI --> Expr ; 
ExprList --> ES0*
```
The presence of the left-hand side label will cause the AST generator to 
create an AST representation for the semicolon (assuming that is what's
desired).

The type rustlr associates with the new non-terminal ES1
will be `Vec<LC<Expr<'lt>>` and semantic actions are generated to
create the vector for both ES1 rules.  A **`+`** means one or more
`ES1` derivations, producing the same vector type, and a **`?`** will
mean one or zero derivations with type `Option<LBox<Expr<'lt>>>`.

Another way to generate the AST for `ExprList` is to manually define the type
of `ExprList`, from which Rustlr will infer that it is a `pass-thru` case.
No type will be created for `ExprList` as it would inherit the type of
the right-hand side of its lone production rule.
```
nonterminal ExprList Vec<LC<Expr<'lt>>>
ExprList --> (LetExpr ;)*
```
This is because rustlr generates an internal non-terminal to represent the right-hand side `*` expression and assigns it type `Vec<LC<Expr<'lt>>>`.
It then recognizes that this is a pass-thru case: the only symbol on the
right, which is of the same type as the left-hand side nonterminal `ExprList`
as declared. This rule will be given an action equivalent to
`ExprList --> (Expr ;)*:v {v}`

Another restriction is that the symbols `(`, `)`, `?`, `*` and `+` may not
be separated by white spaces since that would confuse their interpretation
as independent terminal symbols.  For example, `( Expr ; ) *` is not valid.

#### **Special Operators**

In addition to the `*`, `+` and `?` suffixes, rustlr also recognizes (non-nested)
suffixes such as **`<Comma*>`** or **`<;+>`**.  Assuming that
`Comma` is a declared terminal symbol of the grammer, the expression
`Expr<Comma+>` represents a sequence of one or more Expr separated by Comma,
but not ending in Comma, e.g *a,b,c* but not *a,b,c,*.  In contrast,
`(Expr Comma)+` means that the expression must end in a Comma.  `<Comma*>`
allows the sequence to be empty. The AST generator will also create vectors
as the semantic values of such expressions.  Please avoid whitespaces in
these expressions: `<Comma *>` is not recognized.
For example,
```
  ExprList --> LetExpr<;+> ;?
```
would accept a semicolong-separated sequence of one or more `LetExpr` with an
optional semicolon at the end.

#### Nesting Restriction

The regex operators **cannot be nested.** It is unlikely that
Rustlr will ever allow their nesting for such combinations can easily 
be ambiguous.  For example, with **`(a?)+`**, even a single `a`
will have an infinite number of derivations: as one `a?` or as three,
for example.

In general, be warned that overusing these regex-like operators,
especially in the same production, can easily lead to new
non-determinisms in the grammar.  The new productions generated for
these operators could lead to additional Shift-Reduce and even
Reduce-Reduce conflicts.  For example, a production with right-hand
side **`Expr<Comma*> Comma?`** will lead to a shift-reduce conflict.
However, **`Expr<Comma+> Comma?`** is fine and represents a
comma-separated sequence with an optional trailing comma.

Rustlr does its best to try to prevent the regex operators from causing
ambiguity.  For example, it caches the uses of the operators to avoid
duplicate rules that are almost certain to cause conflicts.
However, mixing regular expressions and context-free grammars will still
have unexpected consequences for the user.  We never have to worry about
a regex being "ambiguous" because they all reduce to a normal form (a
deterministic finite automaton with minimal states).  The same is not
true for grammars.  Grammars cannot be *combined* like regex can: for
example, a rule like `A --> B* B*` is hopelessly ambiguous as
there is no way to determine where the first sequence of B's should stop 
and the second one begins.

The motivation for adding regex-like operators to a parser generator
are both usability and improved ability in creating abstract syntax
automatically.  But much work needs to be done before we can parse
arbitrary EBNF syntax.  We are currently exploring extensions of LR
parsing including *delayed reductions*, which can potentially allow
non-ambiguous grammars to be more easily composed without running into
new conflicts: see the [Appendix][appendix] of this tutorial for experimental
features.



----------------

### 8. JSON Parser

To give another, complete example of the features described in this chapter,
we build a parser for JSON.  The grammar is as follows
```
# Rustlr Grammar for JSON
auto
lifetime 'lt
lexterminal LBRACE {
lexterminal RBRACE }
lexterminal LBRACK [
lexterminal RBRACK ]
lexterminal LPAREN (
lexterminal RPAREN )
lexterminal COLON :
lexterminal COMMA ,
lexterminal NULL null
lexterminal MINUS -
valueterminal TRUE~ bool~ Alphanum("true")~ true
valueterminal FALSE~ bool~ Alphanum("false")~ false
valueterminal STRING~ &'lt str~ Strlit(n)~ &n[1..n.len()-1]
valueterminal NUM~ i64~ Num(n)~ n
valueterminal FLOAT~ f64~ Float(n)~ n
valueterminal BIGNUM~ &'lt str~ BigNumber(n)~ n

nonterminal Integer i64
nonterminal Floatpt f64
nonterminal Boolean bool
nonterminals Value KeyValuePair Number
nonterminal Object : Value
nonterminal List : Value
nonterminal Boolean : Value
topsym Value
resync COMMA RBRACK RBRACE

Integer --> MINUS?:m NUM:n {if m.is_some() {n*-1} else {n}}
Floatpt --> MINUS?:m FLOAT:n {if m.is_some() {-1.0*n} else {n}} 
Number:Bignum --> MINUS?:m BIGNUM
Number:Int --> Integer
Number:Float --> Floatpt
Boolean --> TRUE | FALSE

Value:Number --> Number
Value:Boolean --> Boolean
Value:Str --> STRING
Value --> Object
Value --> List
Value --> NULL
Value --> LPAREN Value RPAREN
KeyValuePair --> STRING COLON Value
List:List --> LBRACK Value<COMMA*> RBRACK
Object:Object --> LBRACE KeyValuePair<COMMA*> RBRACE
```

The grammar is a hybrid with some manually defined types and actions
along with automatically generated ones.
The AST types that are created by this grammar are
```
#[derive(Debug)]
pub enum Number<'lt> {
  Float(f64),
  Int(i64),
  Bignum{m:Option<()>,_item1_:&'lt str},
  Number_Nothing,
}
impl<'lt> Default for Number<'lt> { fn default()->Self { Number::Number_Nothing } }

#[derive(Debug)]
pub enum Value<'lt> {
  Boolean(bool),
  Number(Number<'lt>),
  Str(&'lt str),
  NULL,
  Object(Vec<LBox<KeyValuePair<'lt>>>),
  List(Vec<LBox<Value<'lt>>>),
  Value_Nothing,
}
impl<'lt> Default for Value<'lt> { fn default()->Self { Value::Value_Nothing } }

#[derive(Default,Debug)]
pub struct KeyValuePair<'lt>(pub &'lt str,pub LBox<Value<'lt>>,);
```

The following is the Debug-output of a sample AST produced by the parser,
from which anyone familiar with JSON can surely discern the original source:
```
Object([KeyValuePair("firstName", Str("John")), KeyValuePair("lastName", Str("Smith")), KeyValuePair("isAlive", Boolean(true)), KeyValuePair("age", Number(Int(27))), KeyValuePair("address", Object([KeyValuePair("streetAddress", Str("21 2nd Street")), KeyValuePair("city", Str("New York")), KeyValuePair("state", Str("NY")), KeyValuePair("postalCode", Str("10021-3100"))])), KeyValuePair("phoneNumbers", List([Object([KeyValuePair("type", Str("home")), KeyValuePair("number", Str("212 555-1234"))]), Object([KeyValuePair("type", Str("office")), KeyValuePair("number", Str("646 555-4567"))])])), KeyValuePair("children", List([Str("Catherine"), Str("Thomas"), Str("Trevor")])), KeyValuePair("spouse", NULL)])
```

Finally, here is an alternative way to parse JSON objects into
Rust HashMaps.  We can override not only the type to be generated but the
semantic action as well.  Modify the declarations and rules for Object as
follows:
```
$use std::collections::HashMap;
nonterminal Object HashMap<&'lt str, LBox<@Value>>

Object ==> LBRACE KeyValPair<COMMA*>:entries RBRACE {
  let mut kvmap = HashMap::new();
  for (mut lbx) in entries {
    if let KeyValPair(k,v) = lbx.take() { kvmap.insert(k,v); }
  }
  kvmap
  } <==
```
The `$` directive is similar to `!`, except that it adds the verbatim line
only to the generated AST file as opposed to parser file. The syntax
**`@Value`** refers to the type of the `Value` nonterminal (or you can say
`Value<'lt>`, but you will in general know what type would be generated
by the system for the nonterminal: the `@` symbol offers a convenience.)

Each key-value pair inside the vector created by `<COMMA*>` is wrapped
inside an LBox.  We can take the value from the box with
[LBox::take][take] (which leaves a default value inside the box).

The debug output of the AST from the same JSON source would now be:
```
Object({"spouse": NULL, "age": Number(Int(27)), "phoneNumbers": List([Object({"type": Str("home"), "number": Str("212 555-1234")}), Object({"number": Str("646 555-4567"), "type": Str("office")})]), "children": List([Str("Catherine"), Str("Thomas"), Str("Trevor")]), "lastName": Str("Smith"), "address": Object({"streetAddress": Str("21 2nd Street"), "city": Str("New York"), "state": Str("NY"), "postalCode": Str("10021-3100")}), "isAlive": Boolean(true), "firstName": Str("John")})
```

Although we may at times want to insert such overrides, most of the
automatically generated portions remain usable.

Here are links to the [grammar](https://cs.hofstra.edu/~cscccl/rustlr_project/jsontypes/json.grammar), [parser](https://cs.hofstra.edu/~cscccl/rustlr_project/jsontypes/src/jsonparser.rs), [ast types](https://cs.hofstra.edu/~cscccl/rustlr_project/jsontypes/src/json_ast.rs) and [main](https://cs.hofstra.edu/~cscccl/rustlr_project/jsontypes/src/main.rs) of the project, which may differ slightly
from the above.



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
[regex]:https://docs.rs/regex/latest/regex/
[appendix]:https://chuckcscccl.github.io/rustlr_project/appendix.html
[take]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LBox.html#method.take
[c11]:https://cs.hofstra.edu/~cscccl/rustlr_project/cparser/cauto.grammar
[appendix]:  https://cs.hofstra.edu/~cscccl/rustlr_project/appendix.html
[box]: https://doc.rust-lang.org/std/boxed/struct.Box.html
[lc]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LC.html

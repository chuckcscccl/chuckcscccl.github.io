## Chapter 1: Simple Calculator.

Please note that this tutorial is for **[Rustlr version 0.4][drs]**.
The [older tutorial](https://cs.hofstra.edu/~cscccl/rustlr_project/)
is still available.  Rustlr remains compatible with older grammars,
but some may need to be recompiled.

This tutorial is not an introduction to LR parsing, though only minimal
knowledge of the concept is required to understand the examples.
The first chapter of the tutorial will present three versions of a
simple grammar illustrating the core features of Rustlr, while
additional features and options will be presented in subsequent chapters.

The first grammar parses integer arithmetic expressions and computes their
values.

```ignore
auto
nonterminal E i32
nonterminal T i32
nonterminal F i32
terminals + * - / ( )
valueterminal num ~ i32 ~ Num(n) ~ n as i32
topsym E

E --> E:e + T:t { e + t }
E --> E:e - T:t { e - t }
E --> T
T --> T:a * F:b { a*b }
T --> T:a / F:b { a/b }
T --> F
F --> - F:a { -a }
F --> ( E )
F --> num
```
These are the contents of a Rustlr grammar file, [calc1.grammar](https://github.com/chuckcscccl/rustlr/blob/main/examples/test1/calc1.grammar).
This classic example of an unambiguous grammar is found in most
compiler textbooks.  Rustlr recognizes operator precedence and associativity
declarations (see below), but we start with a proper grammar.
After you **`cargo install rustlr`** you can produce a LALR parser from this
grammar file, which must end with `.grammar`, with:

>  **`rustlr calc1.grammar`**

This runs rustlr in its default LALR(1) mode.  This generates two
files: **`calc1parser.rs`** and **`calc1_ast.rs`** in the working
directory, although the second file won't contain much for this
example.  It will derive the name of the grammar (test1) from the file
path, unless there is a declaration of the form

>  `grammarname somename`

in the grammar spec. The parser must import some elements of rustlr so it
should be used in a crate.  We will come back to how to use the
generated parser later.

####  **GRAMMAR FORMAT**

The first line in the grammar specification:
>  `auto`

is recommended in most situations.
Specifically it means to generate the abstract syntax types and semantic actions
automatically while allowing for overrides.  We provided overrides for the
types of the three non-terminal symbols E, T and F to be i32 and manually
written semantic actions to create values of that type.  However, the
`auto` mode was still useful in that other actions were created automatically.
For example, we did not have to write `F --> ( E:a ) { a }`.

Leaving out `auto` enables some deprecated features of Rustlr and is
generally not recommeneded. The auto mode allows any degree of manual
override, rendering the non-auto mode redundant.  One possible
exception for the non-auto mode is to use a lexical scanner separate
from the built-in one.

RustLr requires that all grammar symbols be declared as terminal or
non-terminal before any production rules.


####  **Top Nonterminal**
>  `topsym E`

Alternatively, `startsymbol E`.  One particular non-terminal symbol
should be designated the top/start symbol: The parser generator will
always create an extra production rule of the form `START --> topsym EOF`

####  **Grammar Production Rules**

You will get an error message if the grammar symbols are not defined before
the grammar rules.  Each rule is indicated by a non-terminal symbol followed
by `-->`, or  `==>`.  The symbol `==>` is for rules that span multiple
lines that you will find used
in other grammars (later chapters).  You can specify multiple production
rules with the same left-hand side nonterminal using |  which you will
also find in other grammars.

The right hand side of each rule must separate each grammar symbol with
whitespaces.  For each grammar symbol such as E, you can optionally
bind a "label" in the form `T:a` or `T:[a]`. The first grammar will only use
the first type of label and we shall explain the other with the next grammar.

The right-hand side of a rule may be empty, which will make the
non-terminal on the left side of `-->` "nullable".
           
####  **Semantic Actions**

Every terminal and non-terminal symbol is associated with the type of
a semantic value.  In the `auto` mode types and semantic actions are
always automatically created unless overrides are provided.

> **All such types are expected to impl `Default` and `Debug`**.

Rustlr parsers will attempt to construct partial semantic values even
in case of parsing errors, hence the requirement for the `Default` trait.

Every production rule can optionally end with a manually written
semantic action inside { and }, which can only follow all grammar
symbols making up the right-hand side of the production rule.  This is
a piece of Rust code that will be injected *verbatim* into the
generated parser, and is expect to return a value associated with the
left-hand side non-terminal of the rule.  The semantic action code
will have access to any labels associated with the symbols defined
using ":".


#### **Creating a Lexical Scanner for the Terminal Symbols**

A lexical scanner is automatically created from the declarations of
the terminal symbols of the grammar.  These terminals can be
categorized into ones that do not carry meaningful semantic values,
and ones that do.  Those that do not carry values are assigned type
`()`, as in unit, in the `auto` mode, and can be declared in one of
two ways:

  1.  If the name of the terminal symbol is the same as its textual form
  (how it appears in concrete syntax), then it can be declared on a `terminals`
  line as in this grammar.  Multiple `terminals` lines are allowed.
  2.  If the name of the grammar symbol is different from its textual form,
  it should be declared on a line such as
  ```
    lexterminal COLON :
  ```
  In the grammar's production rules, the terminal should be referred to as
  `COLON`.  Certain reserved symbols such as `:`, `|` and `{`, `}`
  must be declared this way.

Terminal symbols that carry meaningful values, such as alphanumeric
identifiers, numerical constants and string literals, should be define
with a **`valueterminal`** line as in the defininition of `num` in this grammar.
The format of this line is as follows:

>  valueterminal terminal_name ~ terminal_type ~ expected_token ~ token_value

Each component is separated by a `~`, which is not a Rust operator.
The *token_value* must be of type *terminal_type.*  The last
two components will be used form a 'match' clause, so we can write,
for example,
```
  valueterminal Positive ~ u64 ~ Num(n) if n>0 ~ n as u64
```

The token category **`Num`** is a variant of [RawToken][rtk], the type of token
produced by the built-in generic tokenizer, [StrTokenizer][1].
Other common categories include **Alphanum** for alphanumeric sequences,
**Symbol** for non-alphanumeric symbols, and **Strlit** for string literals.
Consult the [next chapter][chap2] for their usage.

The generated lexer is a struct called calc1lexer alongside the make_parser()
function inside the generated parser file.  One creates a mutable instance
of the lexer using the generated **`calc1lexer::from_str`** and **`test1lexer::from_source`** functions.

#### **Invoking the Parser**

Here is a "main" that creates and invokes the parser.
```
mod calc1parser;
use calc1parser::*;
mod calc1_ast;
fn main() {
  let mut input = "5+2*3";
  let args:Vec<String> = std::env::args().collect(); // command-line args
  if args.len()>1 {input = &args[1]; }
  let mut parser1 = make_parser();
  let mut tokenizer1 = calc1lexer::from_str(input);
  let result = parse_with(&mut parser1, &mut tokenizer1)
               .unwrap_or_else(|x|x);
  println!("result after parsing {}: {}",input,result);
```
The main.rs should be placed in a cargo crate with **"rustlr = "0.4"** in its
dependencies. The files produced by rustlr for the grammar should also be
inside the `src/` folder of the crate.
 The function `parse_with` is created *for each grammar*,
and returns a `Result<T,T>` where `T` is the semantic value type of the
"topsym" (startsymbol) of the grammar. 

This main expects a command-line argument.  Alternatively, 
we can create a lexer from a file source with:
```
  let source = rustlr::LexSource::new("file path").unwrap();
  let mut tokenizer1 = calc1lexer::from_source(&source);
```

#### **Operator Precedence and Associativity Declarations**


An alternative way to write the above grammar is the following
```
auto
nonterminal E i32
terminals + * - / ( )
valueterminal num ~ i32 ~ Num(n) ~ n as i32
topsym E
left + 100
left - 100
left * 200
left / 200

E --> E:e + E:t { e + t }
E --> E:e - E:t { e - t }
E --> E:a * E:b { a*b }
E --> E:a / E:b { a/b }
E(300) --> - E:a { -a }
E --> ( E )  |  num
```
Operators can be declared to be `left` or `right` associative or
`nonassoc`, with a number indicating the precedence level.  Rustlr
does not report shift-reduce conflicts if they are clearly resolved by
such declarations (except at trace level 5 or above).  Although
attractive as they enable ambiguous grammars to be written,
over-reliance on these declarations should be avoided.  For larger
grammars, overusing these declarations can cause problems *(if you find
yourself declaring the precendence of a bracket such as '[', you're in
trouble).*  Even in this small grammar, care must be taken. Unary
operators usually bind tighter than binary ones: the **unary -**
should have precedence over `*` and `/` (the outcome may be the same but 
the manner of evaluation is not).  Thus the *rule* for unary
minus overrides the declared precedence of `-`.  Each grammar symbol
is given a precedence level, which is by default zero.  Each *rule* is
assigned a precedence equal to the symbol on the right-hand side with
the highest precedence, unless overridden as in the unary minus rule.

One place where precedence declarations are arguably justified is the infamous
"dangling else" problem:
```
Statment --> if ( Expression ) Statment
Statment --> if ( Expression ) Statment else Statment
```
Rewriting this grammar unambiguously would require extra rules for all other
cases of `Statment`.  Rustlr allows this problem to be solved by assigning
'else' a higher precedence than 'if', which would force a shift and associate
each 'else' with the nearest 'if'.


### Generating Abstract Syntax Trees

The first versions of the grammar computed numerical values directly, but
more generally parsers create AST structures.  
```
# calc2.grammar
auto
nonterminal E
nonterminal T : E
nonterminal F : E
terminals + * - / ( )
valueterminal num ~ i32 ~ Num(n) ~ n as i32
startsymbol E

E:Plus --> E + T
E:Minus --> E - T
E --> T
T:Times --> T * F
T:Divide -->  T / F
T --> F
F:Neg --> - F
F:Val --> num
F --> ( E )
```
Lines in the grammar that begin with `#` are used for comments. (but
the `#` must be at the very start of the line).

Unlike the first grammar, there are no overrides for the types of non-terminals
nor are there manually written semantic actions.  However, the grammar was
carefully written to distinguish *parse trees* from *abstract syntax trees*.
Normally, Rustlr creates enums for nonterminals that have multiple productions
and structs for nonterminals that have a single production.  However, 
the non-terminals `T` and `F` are only meaningful at the parsing stage and
should not be included in the AST.  This is accomplished with declarations
such as `nonterminal T : E`, which means that the variants for T should be
absorbed into those for E.  Furthermore, the left-hand side of each rule
can also be given a label: this label will become the name of the enum variant
for that case.  The rules without labels, such as `F --> ( E )` are called
"pass-thru" cases and do not generate separate variants: the semantic value
of the left-hand side non-terminal is directly inherited from the sole
right-hand side symbol that is not of unit type.  

The structure generated for E, in `calc2_ast.rs`, is
```
#[derive(Debug)]
pub enum E {
  Plus(LBox<E>,LBox<E>),
  Minus(LBox<E>,LBox<E>),
  Divide(LBox<E>,LBox<E>),
  Times(LBox<E>,LBox<E>),
  Val(i32),
  Neg(LBox<E>),
  E_Nothing,
}
impl Default for E { fn default()->Self { E::E_Nothing } }
```
The variant `E_Nothing` is created to implement the `Default` trait, which is
required of all semantic value types.

#### What is an **LBox?**

In a typical compiler/interpreter, the most meaningful error messages,
such as using a variable out of scope, or calling a function with the
wrong number or type of arguments, are not parsing errors but are only
recognized in later stages, such during type checking. However, all
error messages must indicate the location in the orignal text (line
and column numbers) where the errors originate.  This implies that the
parser must insert this location information into the ASTs that are
created.  All data structures designed for the abstract syntax must
accommodate this information, which can become rather intrusive.
Rustlr tries to
make this process transparent by defining a *custom smart pointer*
called **[LBox][2]**.  It takes advantage of the fact that ASTs are
almost always recursive, which would require smart pointers in the
type definitions. Rustlr computes a reachability closure to determine
where these pointers are required.  However, instead of using a
regular Box, an LBox includes a Box along with the line and column
numbers of where the construct starts.  This information is automatically
inserted into each LBox by the Rustlr runtime parser ([ZCParser][zcp]). 
[Lbox][2] implements `Deref` and
`DerefMut` so they can be used just like a regular Box, *except* when we 
actually need the line/column information:
```rust
pub fn eval(expr:&E) -> Option<i32> {
  use E::*;
  match expr {
    Val(n) => Some(*n),
    Neg(n) => eval(n).map(|x|-x),
    Plus(a,b) => eval(a).map(|x|eval(b).map(|y|x+y)).flatten(), //monadic join
    Minus(a,b) => eval(a).map(|x|eval(b).map(|y|x-y)).flatten(),
    Times(a,b) => eval(a).map(|x|eval(b).map(|y|x*y)).flatten(),
    Divide(a,b) => eval(b).map(|y|{ 
      if y==0 {
        eprintln!("Division by zero line {}, column {}",b.line(),b.column());
        None
      } else { eval(a).map(|x|x/y) } }).flatten(),
    _ => None,
  }//match
}//eval
```

We can inject verbatim Rust code into the parser directly by prefixing each
line with `!`.  Similarly, we can inject code into an `ast.rs` file by prefixing
each line with `$`.  However, typically these will be `use` clauses, as in
```
!use std::collections::HashMap;
```

<br>

This chapter serves as an introduction.  There are other details and features
that will be explained fully in subsequent chapters.


-----------

[1]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.StrTokenizer.html
[2]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LBox.html
[3]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LRc.html
[4]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.ZCParser.html#method.lbx
[5]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.StackedItem.html#method.lbox
[sitem]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.StackedItem.html
[oldchap1]:https://cs.hofstra.edu/~cscccl/rustlr_project/test1grammar.html
[oldchap4]:https://cs.hofstra.edu/~cscccl/rustlr_project/chapter4.html
[lexsource]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.LexSource.html
[drs]:https://docs.rs/rustlr/latest/rustlr/index.html
[tktrait]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/trait.Tokenizer.html
[tt]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html
[rtk]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/enum.RawToken.html
[nextsymfun]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/trait.Tokenizer.html#tymethod.nextsym
[zcp]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.ZCParser.html
[fromraw]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.from_raw
[ttnew]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.new
[regex]:https://docs.rs/regex/latest/regex/
[chap1]:https://chuckcscccl.github.io/rustlr_project/chapter1.html
[chap2]:https://chuckcscccl.github.io/rustlr_project/chapter2.html
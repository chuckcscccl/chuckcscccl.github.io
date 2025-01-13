## Chapter 1: Simple Calculator.

Please note that this tutorial is updated for the latest version of
[Rustlr](https://crates.io/crates/rustlr).  There's an [older
tutorial](https://cs.hofstra.edu/~cscccl/rustlr_project/index3.html)
that was written prior to Rustlr Version 0.4 that is still available.
Rustlr remains compatible with older grammars, but some may need to be
recompiled.

This tutorial is not an introduction to LR parsing, though only minimal
knowledge of the concept is required to understand the examples.
The first chapter of the tutorial will present three versions of a
simple grammar illustrating the core features of Rustlr, while
additional features and details will be presented in subsequent chapters.

---------------

### Installing Rustlr

The rustlr crate contains the parser generation routines as well as
base runtime parsing routines required by all parsers.  Either both
are installed together, or the runtime parsing routines can be
installed alone.  To generate parsers, you can either

  1. install rustlr as a command-line application: **`cargo install rustlr`**.
     
  2. Call the rustlr crate's [generate](https://docs.rs/rustlr/latest/rustlr/fn.generate.html) function from within a rust program, after installing rustlr
  as part of your crate (`cargo add rustlr`).  The function takes in a 
  string slice the same arguments as the command-line application.  However,
  when given the `-trace 0` option, rustlr produces no output during parser
  generation, saving instead all messages in a script.

After a parser has been generated for another application, rustlr can be
installed in that application's crate with only the runtime parsing routines
by either

  1. **`cargo add rustlr --no-default-features`**
  
  2. including the following in Cargo.toml:
   ```
      [dependencies]
      rustlr = { version = "0.6", default-features = false }
   ```
Turning off default features excludes the parser generation routines, making
for a much smaller build.

Optional Feature **`legacy-parser`**:  grammars and parsers for very early
versions of Rustlr require the `legacy-parser` feature.  This feature can
be installed with or without the parser generation routines.  For example,
if you only need the legacy runtime parser, add the following to Cargo.toml:
   ```
      [dependencies]
      rustlr = { version = "0.6", default-features = false, features = ["legacy-parser"] }
   ```

---------------

#### A Rustlr Grammar

Rustlr uses its own syntax for grammars.  However, given a Yacc/Bison
style grammar in a file with a `.y` suffix, rustlr can convert it
into its own syntax while stripping away semantic actions and other
language-specific content.  The new grammar will be saved at the same
location with a `.grammar` suffix, which can then be further enhanced
with rustlr-specific features.  All examples are in the rustlr custom
syntax for grammars, which is richer than that allowed by Yacc/Bison.

The first grammar parses integer arithmetic expressions and computes
their values.

```ignore
auto
nonterminal E i32
nonterminal T i32
nonterminal F i32
terminals + * - / ( )
valterminal num i32
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
grammar file, which must end with `.grammar`, by running the rustlr executable with:

>  **`rustlr calc1.grammar`**

Alternatively, rustlr can be invoked from another rust program by
calling the [rustlr::generate](https://docs.rs/rustlr/latest/rustlr/fn.generate.html) function, which takes the same arguments as the rustlr executable.
Using rustlr this way gives the option of supressing all output to stdout and
stderr with the `-trace 0` option:
```
  let report = rustlr::generate("calc1.grammar -trace 0");
```
The `generate` function returns an a `Result<String,String>` containing either
a log of events on success, or error messages on failure.
With the rustlr command-line application, some messages will always be
printed to stdout/stderr even with the `-trace 0` option.

By default, rustlr tries to generate a LALR(1) parser (modified with `-lr1` and `-lrsd` options). Two files are generated:
**`calc1parser.rs`** and **`calc1_ast.rs`** in the working
directory, although the second file won't contain much for this
example.  The output directory can be changed with the `-o` option, such as
in `rustlr calc1.grammar -o calc1crate/src/`.
It will derive the name of the grammar (calc1) from the file
path, unless there is a declaration of the form

>  `grammarname somename`

in the grammar spec. The parser must import some elements of rustlr so it
should be in a crate.  We will come back to how to run the
generated parser later.


####  **GRAMMAR FORMAT**

The first line in the grammar specification:
>  `auto`

is recommended in most situations.
Specifically it means to generate the abstract syntax types and semantic actions
automatically while allowing for overrides.  We provided overrides for the
types of the three non-terminal symbols E, T and F to be i32 and manually
wrote semantic actions to create values of that type.  However, the
`auto` mode was still useful in that other actions were created automatically.
For example, we did not have to write `F --> ( E:a ) { a }`.

Leaving out `auto` enables some deprecated features of Rustlr and is
generally not recommeneded. The auto mode allows any degree of manual
override, rendering the non-auto mode redundant.  One possible
exception for the non-auto mode is for using a lexical scanner separate
from the built-in one.

Rustlr requires that all grammar symbols be declared as terminal or
non-terminal before any production rules.


####  **Top Nonterminal**
>  `topsym E`

Alternatively one can write `startsymbol E`.  One non-terminal symbol
should be designated the top/start symbol.

####  **Grammar Production Rules**

You will get an error message if the grammar symbols are not defined before
the grammar rules.  Each rule is indicated by a non-terminal symbol followed
by `-->`, or  `==>`.  The symbol `==>` is for rules that span multiple
lines that you will find used
in other grammars (later chapters).  You can specify multiple production
rules with the same left-hand side nonterminal using `|`  which you will
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
automatically created unless overrides are provided.

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
using ":" as mutable variables.


#### **Creating a Lexical Scanner for the Terminal Symbols**

A lexical scanner is automatically created from the declarations of
the terminal symbols of the grammar in the auto mode.  These terminals can be
categorized into ones that do not carry meaningful semantic values,
and ones that do.  Those that do not carry values are assigned type
`()`, as in *unit*, in the `auto` mode, and can be declared in one of
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
  `COLON`.  **Certain reserved symbols including `:`, `|`, `%`, `-->`, `#` and `{`, `}`
  must be declared using `lexterminal`.**

Terminal symbols that carry the most common types of values, including
alphanumeric identifiers, numerical constants (without type suffixes)
and string literals, can be defined with a `valterminal` declaration.
Valid `valterminal` declarations include
```
valterminal INT i64
valterminal FLOAT f32
valterminal IDENTIFIER alphanumeric
valterminal STRING string literal
```
Beside the special descriptions "alphanumeric" and "string literal",
`valterminal` can also name one of the following numerical types:
i8-i64, u8-u64, f32, f64, isize and usize.  Please note that only one
integer type can be assigned to a terminal
symbol using `valterminal` and likewise for floating point types (but see below).

The **`valterminal`** directive is designed to simplify the specification
of a lexer for the most common types of tokens using rustlr's built-in
tokenizer.  A `valterminal` declaration is actually the simplified form of
a longer, `valueterminal` declaration.
**`valterminal num i32`** is equivalent to
```
valueterminal num ~ i32 ~ Num(n) ~ n as i32
```
The format of this line is as follows:

>  *valueterminal terminal_name ~ terminal_type ~ expected_token ~ token_value*

Each component is separated by a `~`, which is not a Rust operator.
The *token_value* must be of type *terminal_type.*  The last
two components will be used to form a 'match' clause, so we can write,
for example,
```
  valueterminal Positive ~ u64 ~ Num(n) if n>0 ~ n as u64
  valueterminal Integer ~ i64 ~ Num(n) ~ n
```
to distinguish special cases. 
The token category **`Num`** is a variant of [RawToken][rtk], the type of token
produced by the built-in generic tokenizer, [StrTokenizer][1], and carries an i64 value.
Other common categories of tokens include **Alphanum** for alphanumeric sequences,
**Symbol** for non-alphanumeric symbols, and **Strlit** for string literals.

Please remember that there is a difference between *token* and *terminal symbol*: a relationship must be established between the two with a *valueterminal* declaration.

Each token carries a `str` slice with the same lifetime as the
input source, which can be defined with a `lifetime` declaration in the grammar.
Essentially, it requires
```
  lifetime 'lt
  ...
  valueterminal Identifier ~ &'lt str ~ Alphanum(n) ~ n
```
The above line is equivalent to **`valterminal Identifier alphanumeric`**.  
If the lifetime is not explicitly declared, it is set to `'input_lt`.
However, naming the lifetime is recommended to avoid potential clashes
with other lifetimes in your program.

For terminals with types that are not recognized by `valterminal`, the
longer `valueterminal` form is required:
```
  valueterminal BOOL ~ bool ~ Alphanum("true") ~ true
  valueterminal BOOL ~ bool ~ Alphanum("false") ~ false
```
These declarations should be placed **before** a generic `alphanumeric`
valterminal declaration.  Alphanumeric terminal symbols declared by `terminals`
automatically take precedence over generic alphanumeric tokens.  For
example: `terminals if else while` means that "else" would be recognized
as the **else** terminal symbol, carrying no value, as opposed to an
`Indentfier` that carries an alphanumeric string slice.

It is also possible to specify custom token categories using regular
expressions.  Consult the [next chapter][chap2] for fuller details on
how to configure the lexical scanner.

The generated lexer is a struct called calc1lexer alongside the make_parser()
function inside the generated parser file.  One creates a mutable instance
of the lexer using the generated **`calc1lexer::from_str`** and **`test1lexer::from_source`** functions.

#### **Invoking the Parser**

Invoking the parser requires instances of the parser and lexer to be created
separately, so that the same parser can parse from multiple sources after
calling `reset`.  
Here is a "main" that creates and invokes the parser.
```
mod calc1parser;
use calc1parser::*;
mod calc1_ast;
fn main() {
  let mut input = "5+2*3";
  let args:Vec<String> = std::env::args().collect(); // command-line args
  if args.len()>1 {input = &args[1]; }
  let mut tokenizer1 = calc1lexer::from_str(input);
  let mut parser1 = make_parser(tokenizer1);
  // parser1.set_err_report(true); // option to log errors instead of printing to stderr
  let result = parse_with(&mut parser1).unwrap_or_else(|x|x);
  println!("result after parsing {}: {:?}",input,result);
  // println!("Error Report: {}", parser1.get_err_report()); // option
}//main
```

Please note that this main expects Rustlr version 0.6+.  In previous
versions the parser and tokenizer interacted in a different way.
The parser now takes posession of the tokenizer.  This allows
semantic actions to directly control the tokenizer.  The tokenizer
can still be accessed independently with the [get_tokenizer](https://docs.rs/rustlr/latest/rustlr/base_parser/struct.BaseParser.html#method.get_tokenizer) function.  The previous (version 0.5) style is still available
by giving rustlr the `-zc` option, though this option may become part of the
`legacy-parser` installation option in the future.

Parser errors are by default printed to stderr.  This behavior can be
changed by calling `set_err_report` on the parser instance, as the
commented-out line indicates.  When given `true` as argument, this
function will log all errors into an internal string, which can then
be retrieved with `get_err_report`.  If `set_err_report` is given
false as argument, it will turn off logging and print to stderr.
Every call to `set_err_report` will **always erase existing error
logs**.

The main.rs should be placed in a crate with **`rustlr =
{version="0.6", default-features=false}`** in its dependencies. The
files produced by rustlr for the grammar should also be inside the
`src/` folder of the crate.
The function `parse_with` is created *for each grammar*,
and returns a `Result<T,T>` where `T` is the semantic value type of the
"topsym" (startsymbol) of the grammar. Even in the event of parsing errors,
a partial result is always returned.

This main expects a command-line argument.  Alternatively, 
we can create a lexer from a file source with:
```
  let source = rustlr::LexSource::new("file path").unwrap();
  let mut tokenizer1 = calc1lexer::from_source(&source);
```

The same parser can be used to parse multiple sources by calling
the [reset](https://docs.rs/rustlr/latest/rustlr/base_parser/struct.BaseParser.html#method.reset) function and by [swapping](https://docs.rs/rustlr/latest/rustlr/base_parser/struct.BaseParser.html#method.swap_tokenizer) in a different tokenizer.


#### **Operator Precedence and Associativity Declarations**


An alternative way to write the above grammar is the following
```
auto
nonterminal E i32
terminals + * - / ( )
valterminal num i32
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
such declarations (except when given the `-trace 5` option).  Although
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
Statement --> if ( Expression ) Statement
Statement --> if ( Expression ) Statement else Statement
```
Rewriting this grammar unambiguously would require extra rules for all other
cases of `Statement`.  Rustlr allows this problem to be solved by assigning
'else' a higher precedence than 'if', which would force a shift and associate
each 'else' with the nearest 'if'.


### Generating Abstract Syntax Trees

The first versions of the grammar computed numerical values directly, but
more generally, parsers create AST structures.  
```
# calc2.grammar
auto
nonterminal E
nonterminal T : E
nonterminal F : E
terminals + * - / ( )
valterminal num i32
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
Lines in the grammar that begin with `#` are used for comments.
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
regular [Box][box], an LBox includes a Box along with the line and column
numbers of where the construct starts.  An additional, *unique identifier* **uid** is
also associated with every LBox so that each AST component can always be
uniquely identified.  All this information is automatically
inserted into each LBox by the Rustlr runtime parser ([BaseParser][zcp]). 
[Lbox][2] implements `Deref` and
`DerefMut` so they can be used just like a regular Box, *except* when we 
actually need the line/column information:
```rust
pub fn eval(expr:&E) -> Option<i32> {
  use E::*;
  match expr {
    Val(n) => Some(*n),
    Neg(n) => Some(-1 * eval(n)?),
    Plus(a,b) => Some(eval(a)? + eval(b)?),
    Minus(a,b) => Some(eval(a)? - eval(b)?),
    Times(a,b) => Some(eval(a)? * eval(b)?),
    Divide(a,b) =>
       eval(b)
       .and_then(|y| {
         if y == 0 {
           eprintln!("Division by zero line {}, column {}",b.line(),b.column());
           None
         } else { eval(a).map(|x| x/y) }
       }),
    _ => None,
  }//match
}//eval
```
Besides [LBox][2], there is also a structure [LC][lc] that implements the
same traits as LBox but contains the lexical location information as well as
the encapsulated expression in the form of an exposed tuple.  It is preferred 
when a smart pointer to heap is not required or redundant.

<br>

We can inject verbatim Rust code into the parser directly by prefixing each
line with `!`.  Similarly, we can inject code into an `ast.rs` file by prefixing
each line with `$`.  However, typically these will be `use` clauses, as in
```
!use std::collections::HashMap;
```

<br>

This chapter serves as an introduction.  There are other details and features
that will be explained fully in subsequent chapters.

**[Chapter 2][chap2]**


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
[zcp]:https://docs.rs/rustlr/latest/rustlr/base_parser/struct.BaseParser.html
[fromraw]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.from_raw
[ttnew]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.new
[regex]:https://docs.rs/regex/latest/regex/
[chap1]:https://chuckcscccl.github.io/rustlr_project/chapter1.html
[chap2]:https://chuckcscccl.github.io/rustlr_project/chapter2.html
[lc]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LC.html
[box]: https://doc.rust-lang.org/std/boxed/struct.Box.html

## Challenging Scenarios

In this chapter we detail a couple of unusual problems and the special
Rustlr features that are designed to resolve them.  Both examples
concern the interaction between the lexical scanner and parser.  LR
parsing requires a *lookahead* symbol to be read by the scanner
independently of what might be expected by a production rule.  But such
independence is not always possible.

The first scenario is found in the [grammar for ANSI
C](https://github.com/chuckcscccl/rustlr/blob/main/examples/cparser/c11.grammar),
which was adopted from a published Yacc version.
```
  typedef unsigned int uint;  // uint tokenized as IDENTIFIER
  uint x = 1;                     // uint tokenized as TYPE_NAME
```

An alphanumeric sequence, such as `uint`, that does not correspond to
a keyword is normally tokenized as an "IDENTIFIER".  However, once the
identifier is subject to a **typedef**, subsequent occurrences of it
are to be tokenized as "TYPE_NAME".  A similar problem occurs with
enum identifiers.  This kind of distinction requires *context
sensitive* information, and indeed, continuing to recognize all
alphanumeric sequences as IDENTIFIER will lead to conflicts in the
grammar.  This scenario suggests that the parser should begin to build
a kind of *symbol table* for the C program, and that the content of
this table should affect the behavior of the lexical tokenizer.

### **`parser.shared_state`**

Every Rustlr parser carries a `shared_state` variable.  The type of
this variable can be specified in the grammar with, for example,
```
  externtype u64
```
The default externtype is `()`.  The specified type
**must implement the Default trait**, which is how the `shared_state`
is initialized.  Furthermore, access to this variable can be shared
(via a `Rc<RefCell<externtype>>` underneath).  As Rustlr generates a
parser and an lexical scanner for each grammar, it also generates code
so that the `shared_state` can be accessed from both.

The `TYPE_NAME` problem is solved using the following declarations in the grammar:

```
$pub use std::collections::HashSet;
$#[derive(Debug,Default)]
$pub struct defined_id_table<'t> {
$  pub typedefs: HashSet<&'t str>,
$  pub enum_ids: HashSet<&'t str>,
$  pub anticipate_typedef: bool,
$  pub in_function:bool,
$  pub warnings_issued:bool,
$}

lifetime 'lt
externtype defined_id_table<'lt>

valueterminal TYPE_NAME ~ &'lt str ~ Alphanum(n) if self.shared_state.borrow().typedefs.contains(n) ~ n
valueterminal IDENTIFIER ~ &'lt str ~ Alphanum(n) ~ n

#production rules:
storage_class_specifier --> TYPEDEF { parser.shared_state.borrow_mut().anticipate_typedef=true; ...}

direct_base ==> IDENTIFIER:id {
 let mut table = parser.shared_state.borrow_mut();
 if table.anticipate_typedef {
    table.typedefs.insert(id);
    table.anticipate_typedef = false;
 }
 ... } <==
```

First, the lines beginning with `$` are injected verbatim into the
abstract syntax module that's to be generated.  Since the struct
definition is `pub`, it will also be visible from the parser module.
The injected lines define the type of the `shared_state`, as specified
by the `lifetime` and `externtype` lines.  This type includes the
stateful information kept by the parser.  In particular, the hashset
`typedefs` and the boolean flag `anticipate_typedef` are used to
resolve the `TYPE_NAME` problem.

Next, the declarations of the `TYPE_NAME` terminal symbol must preceed that
of the `IDENTIFIER` terminal, and the *token form* of `TYPE_NAME` predicates
the `Alphanum(n)` with a condition.  On this `valueterminal` line,
`self.shared_state.borrow()` immutably references the `defined_id_table` struct
shared by the parser and lexer.  In this case, "self" refers to the lexer.
The ordering of the `valueterminal` lines are used to form a `match` clause
that determines the kind of token to be emitted by the lexer.

Finally, the relevant production rules contain manually augmented semantic
actions that access the `parser.shared_state` via `borrow()`/`borrow_mut()`.
Every semantic action is in the context of a function where the variable
`parser` can be referred to.  Up on encountering a `typedef` keyword, the
`anticipate_typedef` flag is set so that the next `IDENTIFIER` recognized will
be recorded in the `typedefs` hashset of the shared state.  This means
that subsequent occurrences of the sequence will be tokenized as `TYPE_NAME`.

Note that the `...` at the end of the semantic actions indicate that the
rest of the action (the part that creates the AST) is to be automatically
generated.

----------------

With the `auto-bump` option for generating bump-allocated ASTS, the shared
state must be accessed as `parser.shared_state.borrow_mut().state`;

----------------

### The Tokenizer's *Priority Multiset*

The second problematic scenario was encountered when adopting a
grammar for an early, pre-generics version of Java to accomodate
generic type syntax.  That is, we wish to parse type expressions such
as
```
   HashSet<String,HashSet<String>>
```

The problem here is that the "`>>`" symbol represents the
*right-shift* operator in Java and in many other languages.  Most
lexical scanners will prefer longer matches over shorter ones, which
means that the `>>` at the end of the type expression will not be
recognized as two distinct tokens without white spaces between them.

Another point to recognize here is that in an LR parser, we cannot
anticipate the next token to be read in the context of a single production
rule.  An LR state consists of many rules (items) and there may be several
anticipated tokens.  Also, the lookahead symbol is read *before* determining
the state transition or action to take.  These factors make it difficult to
automatically customize the tokenizer based on the production rule.

We again need to override some semantic actions and utilize the
`parser.shared_state` to communicate with the lexer.

The built-in, customizable lexer, [StrTokenzier][1], contains a *multiset* of symbols designated as **[priority symbols](https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.StrTokenizer.html#structfield.priority_symbols)**.
A multiset is a set in which there could be multiple occurrences of each element.
The multiset is initially empty.

Normally, [StrTokenzier][1] will prefer longer matches over shorter
ones **unless** a symbol is found in the priority multiset.  The
priority multiset is checked first to see if there are any anticipated
tokens that should be prioritized over the norm.  Each match with some
entry in the multiset decreases the number of occurrences of that
entry in the multiset.

The `>>` problem is solved with the following relevant declarations in the grammar:
```
$#[derive(Debug,Default)]
$pub struct multiswitch(usize); 
$impl multiswitch {
$  pub fn set(&mut self) { self.0 += 1; }
$  pub fn get(&mut self) -> usize {
$    if self.0>0 { self.0 -= 1;  self.0+1 } // auto-decrement
$    else {self.0}
$  }//get
$}//impl multiswitch

externtype multiswitch

lexconditional self.shared_state.borrow_mut().get()>0 ~ add_priority_symbol(">")

# Relevant productions:
type_arguments --> LT flag_state type_arg<COMMA+> GT

flag_state -->  { parser.shared_state.borrow_mut().set(); ... }
```

The terminal symbols `LT` and `GT` represent `<` and `>` respectively.

In this scenario, the `shared_state` between the parser and lexer is a
`multiswitch` struct that contains counter, initially zero by *Default*.
The `get()` function returns the value of the counter and decreases the
counter, so each call to increment the value via `set()` is only usable
once.  The `lexconditional` directive specifies a boolean condition followed
by an action on the lexer (`self` refers to the lexer).
Before each attempt to scan the next token,
the lexer will check each lexconditional statement.  The [StrTokenizer::add_priority_symbol](https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.StrTokenizer.html#method.add_priority_symbol)
function inserts a new symbol into the multiset.

The `flag_state` non-terminal's sole purpose is to inject an additional
semantic action into the `type_arguments` rule.  As `flag_state` is an
empty production, this has the same effect as an intermediate action in
Yacc/Bison. It will only be executed when the prefix to the left,
`LT type_argument<COMMA+>`, is unambiguously confirmed.  The action sets
the multiswitch, which will cause an instance of GT (`>`) to be added to
the lexer's priority multiset.  This instance is automatically removed once
matched.  

Rustlr is structured so that the lexer is not directly controlled by the parser:
the only way for them to communicate is via the `shared_state`.   




--------------------

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
[lc]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LC.html
[box]: https://doc.rust-lang.org/std/boxed/struct.Box.html

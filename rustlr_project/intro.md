# RustLr: LR Parser Generator

## Basic Concepts

  Instead of comprehensive coverage of parsing concepts, we begin with
a review of some of the most important terminology.

### Context Free Grammar

  A context free grammar (CFG) consists of *production rules* formed
of *terminal* and *nonterminal* symbols.  Each production has a
*left-hand side* (LHS) and a *right-hand side* (RHS):

```
   LHS  -->  RHS
```

  Because the grammar is context ***free***, the LHS must consist of
exactly one symbol, otherwise it becomes context *sensitive*.  In
theoretical computer science context sensitive grammars define
languages that are subject to the *Halting Problem*, which technically
means that there is no general way to write a parser generator for all
such grammars.  Although CFGs form the backbone in practical parsing
as well, many parser generators allow customizations that enable the
parsing of some context sensitive languages.

  In contrast to the LHS, the RHS of a rule may consist of zero or
more symbols.  When a symbol only appears on the RHS of productions
and never on the LHS, it is called *terminal,* as in that it cannot
generate further symbols.  If a symbol appears on the LHS of any rule,
it is called *nonterminal*.  A nonterminal symbol may appear on the
LHS or RHS of rules.

  If the RHS of a rule is empty, it is called "nullable."

Each grammar has a designated nonterminal called the *start symbol*.

A grammar defines a language consisting of all sequences of terminal symbols that can be derived from the start symbol.  Each such sequence is called a *sentence* of the language.  More importantly than the language itself,
however, the grammar defines how each sentence in the language is *derived*.
For example, the following grammar, with start symbol `S` and sole terminal
symbol `a`, define the set of all sentences consisting of an **odd**
number of `a`s:
```
  S --> a
  S --> a a S
```
  Note that the first production is *nullable* and specifies that the empty sequence is derivable from `S`.  A derivation of `a a a a a`, for example, is
`S --> a a S --> a a a a S --> a a a a a`.

  The following grammar defines the same language, but not how sentences are derived:
```
  S --> a
  S --> a S a
```
  The same sentence is derived differently with this grammar:
`S --> a S a --> a a S a a --> a a a a a`.  The "big deal" here is that the
first grammar can be parsed from *left to right* while the second one cannot.
Imagine a computer from the 1960s that reads input from a strip of
tape.  We can only read one symbol at a time as we roll the tape.
Although modern computers no longer have such limitations, it's still easier
to process input symbols one at a time.

As we read the input using the first grammar, if we read some symbol
other than 'a', or reach the end of the input prematurely, we can
conclude that the input cannot be parsed.  If we read an 'a', we need
to decide which production rule to apply. For this decision we need to
*lookahead* to the *next* symbol.  If the next symbol is also 'a' we
apply `S --> a a S` and if we reach the end of the input, we apply `S
--> a`.  Thus we can parse the first grammar with *one lookhead*, and
the first grammar is can be classified as an **LR(1)** grammar, as in
*left to right with one lookahead*.  

With the second grammar, however, parsing becomes more complicated.
We cannot know when to apply the first production `S --> a` unless we
know which 'a' is exactly the one in the middle.  But how can we know
that without reading the entire input and counting the symbols in the
first place?  We cannot decide which rule to apply with any fixed,
finite number of lookaheads: this grammar is **not LR(k)** for any k.
The consequence is that it's hard to imagine how we can parse this
sentence with a single pass over the input.  Even with modern
computers, we still wish to be as efficient as possible and minimize
the overhead of parsing.

Thus it is not enough to just write a CFG for an efficient parser to
be generated.  LR parser generators common compute only one lookahead,
and thus it is important to know when a grammar is LR(1).
Fortunately, part of this learning process can be by trail and error,
because the generator will inform you the grammar is not LR(1).  A
theoretical result states that *every deterministic context free
language can be defined by an LR(1) grammar.* Thus it is always
possible to write a parsable grammar for your well-conceived language,
even if it doesn't always give you the same derivations.



## Old Introduction

It's been decades since Donald Knuth introduced LR parsing and proved that
*every deterministic context free language has an LR(1) grammar.*  That is
quite an impressive result, and yet the question as to what is the best
approach to creating parsers has never been settled.  The flavors of parser
generator compete like the kinds of programming languages they help to
implement.  Convincing someone that LR is better than LL (or vice-versa)
is as hard as convincing someone that functional programming is better than
OOP.

Despite the strongest theoretical results, LR parsing requires a
rather steep learning curve when compared to top-down parsing
techniques such as recursive descent.  Recent developments such as
Parsing Expression Grammars (PEGs) has made top-down parsing even more
attractive despite the ever-present problem of left-recursion.

goal1 towards EBNF
goal2 towards 

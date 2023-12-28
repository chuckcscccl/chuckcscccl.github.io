# RustLr: LR Parser Generator

## Basic Concepts

The following is not intended to be a comprehensive review of concepts
related to parsing.  Rather, we review the basic terminology used in
the tutorial and point out some aspects of grammars and parsing for
the users of this tool to keep in mind.

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
as well, many parser generators also allow customizations that enable the
parsing of some context sensitive languages.

  In contrast to the LHS, the RHS of a rule consists of a sequence of
zero or more symbols.  When a symbol only appears on the RHS of
productions and never on the LHS of any production, it is called
*terminal,* as in that it cannot generate further symbols.  If a
symbol appears on the LHS of any production rule, it is called
*nonterminal*.  A nonterminal symbol may appear on the LHS or RHS of
rules.

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
The derivation of `a a a a a`, for example, is
`S --> a a S --> a a a a S --> a a a a a`.

  The following grammar defines the same language:
```
  S --> a
  S --> a S a
```
  But the same sentence is derived differently with this grammar:
`S --> a S a --> a a S a a --> a a a a a`.  The "big deal" here is that the
first grammar can be parsed from *left to right* while the second one cannot.
Imagine a computer from the 1960s that reads input from a strip of
tape.  We can only read one symbol at a time as we roll the tape.
Although modern computers no longer have such limitations, it's still 
more efficient to process the input *in a single pass.*

As we read the input using the first grammar, if we read some symbol
other than 'a', or reach the end of the input prematurely, we can
conclude that the input cannot be parsed.  If we read an 'a', we need
to decide which production rule to apply. For this decision we need to
*lookahead* to the *next* symbol.  If the next symbol is also 'a' we
apply `S --> a a S` and if we reach the end of the input, we apply `S
--> a`.  Thus we can parse the first grammar with *one lookhead*, and
the first grammar can be classified as an **LR(1)** grammar, as in
*left to right with one lookahead*.  Please note that this is a rather
simplified description of LR parsing, which actually generates a state
machine that keeps track of all possible productions to "reduce" by
until it narrows the possibilies to a single choice: for details
readers are encouraged to consult a compiler text.

With the second grammar, however, parsing becomes more complicated.
We cannot know when to apply the first production `S --> a` unless we
know which 'a' is exactly the one in the middle.  But how can we know
that without reading the entire input and counting the symbols in the
first place?  We cannot decide which rule to apply with any fixed,
finite number of lookaheads: this grammar is **not LR(k)** for any k.
With such grammars it's often difficult to parse sentences in a single
pass.  Even with modern computers, we still wish to be as efficient as
possible and minimize the overhead of parsing.

Thus it is not enough to just write a CFG for an efficient parser to
be generated.  LR parser generators commonly compute only one lookahead,
and thus it is important to know when a grammar is LR(1).
Fortunately, part of this learning process can be by trail and error,
because the generator will inform you if the grammar is not LR(1).  A
theoretical result states that *every deterministic context free
language can be defined by an LR(1) grammar.*  Thus it is always
possible to write a parsable grammar for your well-conceived language,
even if it doesn't always give you the same derivations.

### Ambiguous Grammars

A grammar is ambiguous if a sentence can be derived in more than one way.
For example,
```
  S --> c
  S --> S S
```
defines any sequence of one or more `c`s.  However, there are multiple ways (paths) to derive most sentences:
```
  S --> S S --> c S --> c S S --> c c S --> c c S S --> c c c c
  S --> S S --> --> S S S --> S S S S --> c c c c
```
A more practical example of ambiguity is when defining arithmetic expressions:
```
E --> E + E
E --> E - E
E --> E * E
E --> integer
```
This grammar is ambiguous because it does not specify the precedence or
associativity of operators so there are multiple ways to parse, for example,
`3+2*4` and `5-2-1`.  Note that although `+` usually represents an associative
operator, ambiguity persists as far as the grammar is concerned, because
there are still multiple ways to derive a sentence such as `1+1+1`.

Another famous example of ambiguity is the "dangling else problem"
```
   Statement --> if ( Expression ) Statement
   Statement --> if ( Expression ) Statement else Statement
``` 
In a sentence such as `if (a) if (b) c; else d;` the grammar does not
specify which `if` the `else` is associated with.

Ambiguity is sometimes natural for human languages but most computer
languages should avoid them.  There are multiple ways to deal with
ambiguity when writing a grammar, including declaring the
precedence/associativity of operators separately, which is something
that's allowed by rustlr.  Truth be told, it is generally difficult
for humans, even highly educated ones, to write large, unambigous
grammars.  Fortunately, it is another theoretical result that *no
LR(1) grammar is ambiguous*, and thus the parser generator will also
help you construct an unambiguous grammar via trial and error.  Even
with ambiguous grammars, parsers can still be generated by resorting
to default choices, but such an approach may lead to hidden,
unexpected errors in programs later on.



### CFG and Regular Expressions

  In contrast to context free grammars, regular expressions (regex) are
simpler to understand and use.  This is because every regex can be
recognized by a deterministic automaton with a minimal number of
states. Basically, this means that there is no such thing as ambiguity
when writing regex. For example, `a*` and `a*a*` define the same
language (any number of 'a's).  Furthermore, there is no distinction
in how a sentence is recognized because the state machine generated
from them are the same.  There is a standard way to convert a regex to
an equivalent CFG. For example,
```
  A -->
  A --> A a
```
defines `a*`.  But the problem with CFGs is that they cannot be *composed*
the same way as regex. Is the following grammar, which tries to emulate
`a*a*` ambiguous?
```
  S --> A A
  A -->
  A --> A a
```
Yes it is!  How is one to determine where to separate a sequence
of `a`s into two separate ones?  CFGs are not just about recognizing a
sentence, but how it's recognized.  Rustlr does in fact allow the limited
use of some regex-like operators, including * and +, when defining the
grammar.  For example, `S --> a*` is in fact valid.  But when using
these operators it is important to remember this distinction: CFGs
cannot be freely composed.  Most programmers are familiar with the
concept of modularity, of being able to replace one component of a
program with an equivalent one, of combining multiple functions into a
single one, etc.  But that kind of modularity is difficult to achieve
when writing unambiguous CFGs.

### Top-Down and Bottom-Up Parser Generation

  There are two main categories of parser generators: bottom-up and
top-down.  Each has its advantages.  Top-down parsers are easier to
understand, though they come with more restrictions.  Traditionally,
top-down generators require LL(1) grammars. Every LL(1) grammar is also
an LR(1) grammar, but modern top-down generators go beyond a single
lookahead.  Some allow runtime backtracking.  Although backtracking is
not common in practice, and the speed of modern computers mitigate
the cost of backtracking, it is a fact that an LR parser *will never
backtrack.*  Top-down grammars suffer from the problem with left-recursion.
Both left and right-recursive grammar are acceptable for LR parsing, though
left-recursive grammars generate more efficient parsers.

### Hand Written Recursive Descent Parsers

  A popular choice for modern programming language implementations is
to write a recursive-descent, top-down parser by hand.  This approach
gives the parser creator a great amount of flexibility.  One can
decide, for example, exactly how much to look ahead to make a correct
derivation, where to allow backtracking, etc...  This approach also
avoids the theoretical limits of formal languages.  For example,
consider parsing a generic type expression such as
`Hashmap<A,Hashmap<B,C>>`.  The problem we encountered here is that
the lexical tokenizer, which is defined using regular expressions,
will return `>>` as a single token representing the right-shift
operator found in many languages.  Yet in the context of parsing
generic types, `>>` represent two distinct right-angle brackets.  We
have a rather ironic situation where the grammar is context-free but
the lexical scanner is context-sensitive.  When writing the parser by
hand, we known exactly when we're parsing type expressions and can
direct the lexical tokenizer to change its behavior.

It is possible to address these problems in a generated parser.  In
Rustlr, the parser can interact with the tokenizer through its
"semantic actions".  The advantage of a properly written grammar, with
some special actions to deal with these unusual situations, can still
be easier to write and modify compared to a hand-written one.
Grammars are also more language-independent.  The popularity of
hand-written parsers appears to be mainly based on the steep learning
curve required by parser generators, and LR generators in particular.



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

## RUSTLR: Bottom-Up Parser Generator for Rust, Version 0.4.x

[Rustlr](https://crates.io/crates/rustlr) was originally created as a
research platform to explore advanced ideas in grammars and
parsing.  However, rustlr was also designed to be
practcial and can compete with other parser generators
in terms of features and usability.  Among its abilities are

 1. The option of automatically generating the abstract syntax datatypes 
and semantic actions from hints given in the grammar, 
in whole or in part.  The AST types do not necessarily mirror the format
of the grammar: one can declare dependencies that allow multiple non-terminal
symbols to share a common type.

 2. The option of generating recursive AST types using the "bumpalo" crate,
which enables deep pattern matching on recursive structures by replacing
smart pointers with transparent references.

 3. The possibility of using regular-expression style operators such as
*, + and ? directly inside the grammar.  This is not as easy to do as it
sounds as the additional grammar rules required to
support them may lead to new ambiguities.

 4. The option of using a larger class of unambiguous grammars 
compared to traditional LR and LALR, based on [*Selective Marcus-Leermakers*][bns] delayed reductions. See the [Appendix][appendix].
Other experimental features include an "unexpected wildcard" symbol.

 5. The ability to interactively train the parser to give better error messages.

 6. Automatically generates a lexical scanner from the declaration of
   terminal symbols.


Rustlr also implements capabilities found in traditional LR parsing tools
including operator precedence and associativity declarations and error
recovery mechanisms. 
Rustlr is designed for the parsing of programming language
syntax.  It is not designed to parse natural languages, or binary
data, although there's also nothing that prevents it from used for
those purposes.  Rustlr generates parsers for Rust and for F\#
(Microsoft's version of OCaml) and will target other typed, functional
programming languages as these languages often lack choices in parsing
tools.

Documentation on
<a href="https://docs.rs/rustlr/latest/rustlr/">docs.rs</a>
serves as technical reference but not as tutorial.

The tutorial is organized around a set of examples, with each example
explaining a set of more advanced features.


### Tutorial Chapters

The chapters in <b>bold</b> listed below are complete.  The others provide additional examples and generally contain enough comments to be readable.
The first two chapters suffice to give a good working knowledge of the
principal capabilities of rustlr.

<ol>
<li> <b><a href="chapter1.html">Introduction: Basic Calculators</a></b> 
<li> <b><a href="chapter2.html">Advanced Examples</a></b> 
<li> <b> <a href="training.html">Interactive Training for Enhanced Error Reporting</a></b>
<li> <b><a href="bumpast.html">Generating Bump-allocated ASTs</a></b>
<li> <b><a href="errors.html">Error Recovery Options</a></b>
<li> <b>Special Example: <a href="https://github.com/chuckcscccl/rustlr/blob/main/examples/noncf/ncf.grammar">Non-context free language</a>, using External State.</b> Link to <a href="https://github.com/chuckcscccl/rustlr/tree/main/examples/noncf/">full crate</a>
<li> <b>Special Chapter: <a href="chapterfs.html">Generating a Parser for F#</a></b>

<li> <b>Appendix: <a href="appendix.html">Experimental Features (Delayed Reductions and the Wildcard Token)</a></b>
</ol>

#### Additional Chapters Under Construction
<ul>
<li> Advanced Example: <a href="https://github.com/chuckcscccl/rustlr/blob/main/examples/cparser/c11.grammar">Building a Parser for C</a>. (under construction).  <a href="https://github.com/chuckcscccl/rustlr/tree/main/examples/cparser">link to crate</a>

<li> Additional Full Example: <a href="https://github.com/chuckcscccl/rustlr/blob/main/examples/yacc/yacc.grammar">Yacc converter</a>.  Create with rustlr grammar that builds a parser for converting Yacc grammars to Rustlr format, stripping away all C declarations and actions.

<li> Additional Full Example: <a href="https://crates.io/crates/lambdascript">Lambdascript</a>.  Program implementing and tracing beta-reduction
steps for the untyped lambda calculus.  This crate was created using rustlr 
and <a href="https://cs.hofstra.edu/~cscccl/rustlr_project/lambdascript/untyped.grammar">this grammar</a>.
</ul>

<p>
<H3>References</H3>

<p>
</BODY> 
</HTML>


[1]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.StrTokenizer.html
[2]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LBox.html
[3]:https://docs.rs/rustlr/latest/rustlr/generic_absyn/struct.LRc.html
[4]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.ZCParser.html#method.lbx
[5]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.StackedItem.html#method.lbox
[sitem]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.StackedItem.html
[chap1]:https://chuckcscccl.github.io/rustlr_project/chapter1.html
[chap2]:https://chuckcscccl.github.io/rustlr_project/chapter2.html
[appendix]: https://chuckcscccl.github.io/rustlr_project/appendix.html
[lexsource]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.LexSource.html
[drs]:https://docs.rs/rustlr/latest/rustlr/index.html
[tktrait]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/trait.Tokenizer.html
[tt]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html
[rtk]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/enum.RawToken.html
[fromraw]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.from_raw
[nextsymfun]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/trait.Tokenizer.html#tymethod.nextsym
[zcp]:https://docs.rs/rustlr/latest/rustlr/zc_parser/struct.ZCParser.html
[ttnew]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/struct.TerminalToken.html#method.new
[getslice]:https://docs.rs/rustlr/latest/rustlr/lexer_interface/trait.Tokenizer.html#tymethod.get_slice
[bns]:https://hal.archives-ouvertes.fr/hal-00769668/document

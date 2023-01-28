
### 7.Training The Parser For Better Error Reporting

It is recommended that, when a parser is generated, the -trace 3 option is
given, which will print all the LR states that are created. This may be helpful
when training the parser.  Each time the parser is regenerated the states may
have different numbers identifying them, even if the grammar is unchanged.

With a newly generated parser, when a parser error is encountered, the
line and column numbers are printed and an "unexpected symbol" error
message is given. To print more helpful error messages, the parser can
be trained interactively.  Interactive training also produces a script
for future, automatic retraining when a new parser is generated.

As example, one can use the crate created from `calcauto.grammar` in [Chapter 2][chap2].
Modify [main.rs](https://github.com/chuckcscccl/rustlr/blob/main/examples/autocalc/src/main.rs) by changing the call to `parse_with` to
```
  let tree4= calcautoparser::parse_train_with(&mut parser4, &mut scanner4,"src/calcautoparser.rs");
```
For input with no errors, this call works the same way as `parse_with`  The
`parse_train_with` function takes a path to a copy of the parser being trained
(it's not recommended to change the copy that you're using this way).
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
[chap2]:https://chuckcscccl.github.io/rustlr_project/chapter2.html
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

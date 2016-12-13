# goParseBSV
A standalone parser for BSV (Bluespec SystemVerilog) written in Go

Copyright (c) 2016-2017 Rishiyur S. Nikhil. All Rights Reserved.

This is a pure parser, reading in source text file(s) and producing an
AST (Abstract Syntax Tree).  The parser is written in the Go
programming language, and has been tested only with Go 1.7.4 under
Linux.

This parser is intended to enable people to write different back-ends
for BSV, for example to feed into formal verification tools, IDEs,
cross-reference tools, dependency analyzers, etc.

----------------------------------------------------------------

## Example build-and-run


Let's assume you have cloned

        github.com/rsnikhil/goParseBSV/

into

        $(GOPATH)/github.com/rsnikhil/goParseBSV/

In the `goParseBSV/main` directory, do:

        $ make

This will create an executable `goParseBSV`

        $ ./goParseBSV --help

will print a help message.

        $ ./goParseBSV --pp  Foo.bsv

will parse the BSV source file Foo.bsv and will either print an error
message if it encounters a syntax error, or a pretty-printed version
of the parse tree for the AST for the whole file.

        $ ./goParseBSV Foo.bsv

will parse the BSV source file Foo.bsv and just print 'ok' if there are or parse errors.

----------------------------------------------------------------

## Status

It covers most of the BSV language, except:

  * Does not cover `import "BVI"` construct for importing Verilog.

  * Does not cover old-fashioned long-form module instantiation.

  * Does not do preprocessor macro substitution.

  * Does not cover a few other small corner cases.

All these limitations are likely to be fixed as we continue
development.  It does handle `include` and `ifdef macro` constructs,
recursively.

Note, it's a pure parser and does not do other front-end tasks that
are arguably the job of a subsequent pass over the AST:

  * Does not do any desugaring (other than representing unary prefix and binary infix operators as ordinary function calls)

  * Does not do "import chasing"

  * Does not do scope analysis (proper def-use structure)
  * Does not do type-checking

It has been tested successfully on several dozen real BSV source files
that constitute a small SoC: pipelined CPU, caches, interconnect,
memory system, control fabric, UART model, memory model, etc.

Still, it is very preliminary.  It is not very robust about syntax
errors (a bit too liberal in some places) and is intended for now to
be used only on programs that are acceptable to Bluespec's _bsc_
compiler; the main purpose is supplementary processing of BSV
programs, not as a front-line parser.  It is likely to become more
accurate and exact over time.

----------------------------------------------------------------

## Code notes

`grammar.txt` describes the grammar being parsed.

`ast_defs.go` describes the ASTs (Abstract Syntax Trees) that are the output of the parser.

`parser.go` is the top-level file of the parser, and `lexer.go` is the
entire lexical analyzer.  The function `TestParser()` at the bottom of
the file shows an example of creating a lexer from an input filename
and a set of macro definitions, and passing it to `ParsePackage()`,
the top-level function that parses a BSV package (in a file),
returning its AST.  `TestParser()` also calls `AST_pp()` to pretty-print
the parser.

Each syntactic construct has a parsing function, and these are grouped
into major syntactic categories in the other files: `parse_types.go`,
`parse_exprs.go`, `parse_patterns.go`, `parse_stmts.go` and
`parse_FSM.go`.

The parser is a classical, old-fashioned, hand-written,
recursive-descent parser (no parser generators or any such automation).

`backend_pp.go` is an example back-end: a pretty-printer.  You can use
it as a reference for how to traverse an AST when writing new
back-ends for other purposes.

General tip in understanding the code: Go does not have a 'union' or
'tagged union' type, to represent alternatives within a syntactic
category. Instead, Go has a concept of an `interface` type on which
one defines interface methods; we use this as a tagged union.  The
interface type `AST` is defined in `ast_defs.go` for the purposes of
ASTs.  The interface type `ast_pp_ifc` is defined in `backend_pp.go`
for the purposes of the pretty-printer.  We expect that each back-end
can do something similar.

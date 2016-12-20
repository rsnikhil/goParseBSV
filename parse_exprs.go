// Copyright (c) 2016-2017 Rishiyur Nikhil and Bluespec, Inc.  All Rights Reserved.

// Part of goParseBSV, a parser for BSV files
// This part defines parsers for exprs.

package	goParseBSV

import (
	// golang packages
	"fmt"
	"os"
)

// ParseExpr parses a BSV expression
func ParseExpr (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseExpr ...", nil, "\n")

	var result AST

	if TokenIsKeyword (lexer, "return") {
		GetToken (lexer)
		result = & AstReturn { Expr: ParseCondExpr (lexer) }
	} else {
		result = ParseCondExpr (lexer)
	}

	debugPrint (os.Stdout, "ParseExpr ast: ", result, "\n")
	return result
}

// ParseCondExpr parses a conditional expr: e0 ? e1 : e2
func ParseCondExpr (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseCondExpr ...", nil, "\n")
	ast0 := ParseCondPredicate (lexer)
	debugPrint (os.Stdout, "ParseCondExpr ast0: ", ast0, "\n")
	if TokenIsSpecificPunctuation (lexer, "?") {
		var ast AstExpr
		ast.Expr0 = makeIdeFromToken (*lexer.Token, "PrimCond")

		GetToken (lexer)
		ast1 := ParseCondExpr (lexer)
		skipPunctuationMust (lexer, ":")
		ast2 := ParseCondExpr (lexer)

		ast.Exprs = [] AST { ast0, ast1, ast2 }
		return & ast
	} else {
		return ast0
	}
}

// ParseCondPredicate parses the predicate of a conditional
func ParseCondPredicate (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseCondPredicate ...", nil, "\n")

	ast := ParseExprOrCondPattern (lexer)

	if TokenIsSpecificPunctuation (lexer, "&&&") {

		var astCP AstCondPredicate

		astCP.Conjuncts = append (astCP.Conjuncts, ast)

		for TokenIsSpecificPunctuation (lexer, "&&&") {
			GetToken (lexer)
			astJ := ParseExprOrCondPattern (lexer)
			astCP.Conjuncts = append (astCP.Conjuncts, astJ)
		}
		ast = & astCP
	}

	debugPrint (os.Stdout, "ParseCondPredicate: => ", ast, "\n")
	return ast
}

// ParseExprOrCondPattern parses an infix binary-op expression
func ParseExprOrCondPattern (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseExprOrCondPattern ...", nil, "\n")

	astExpr := ParseInfixExpr (lexer, 1)
	if TokenIsKeyword (lexer, "matches") {
		var astCondPattern AstCondPattern
		GetToken (lexer)
		astCondPattern.Expr    = astExpr
		astCondPattern.Pattern = ParsePattern (lexer)
		debugPrint (os.Stdout, "ParseExprOrCondPattern: => ", & astCondPattern, "\n")
		return & astCondPattern
	} else {
		debugPrint (os.Stdout, "ParseExprOrCondPattern: => ", astExpr, "\n")
		return astExpr
	}
	
}

// ParseInfixExpr parses an infix binary-op expression
func ParseInfixExpr (lexer *Lexer, precedence int) (AST) {
	var astL AST

	debugPrint (os.Stdout, "ParseInfixExpr ...", nil, "\n")

	if (precedence == 11) {
		astL = ParseExprUnary (lexer)
	} else {
		astL = ParseInfixExpr (lexer, precedence + 1)
	}

	token := lexer.Token
	s     := token.StringVal
	b     := (precedence == 11) && ((s == "*") || (s == "/") || (s == "%"))
	b      = b || ((precedence == 10) && ((s == "+") || (s == "-")))
	b      = b || ((precedence ==  9) && ((s == "<<") || (s == ">>")))
	b      = b || ((precedence ==  8) && ((s == "<=") || (s == ">=") || (s == "<") || (s == ">")))
	b      = b || ((precedence ==  7) && ((s == "==") || (s == "!=")))
	b      = b || ((precedence ==  6) && (s == "&"))
	b      = b || ((precedence ==  5) && (s == "^"))
	b      = b || ((precedence ==  4) && ((s == "~^") || (s == "^~")))
	b      = b || ((precedence ==  3) && (s == "|"))
	b      = b || ((precedence ==  2) && (s == "&&"))
	b      = b || ((precedence ==  1) && (s == "||"))
	b      = (token.TokType == TokOther) && b

	if b {
		var ast AstExpr
		ast.Expr0 = & (AstIde {LeafValue: token})
		GetToken (lexer)
		ast.Exprs = [] AST { astL, ParseInfixExpr (lexer, precedence) }
		debugPrint (os.Stdout, "ParseInfixExpr: => ", & ast, "\n")
		return & ast
	} else {
		debugPrint (os.Stdout, "ParseInfixExpr: => ", astL, "\n")
		return astL
	}
}

// ParseExprUnary parses a prefix unary-op expression
func ParseExprUnary (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseExprUnary ...", nil, "\n")
	token := lexer.Token
	s     := token.StringVal
	isPrefixOp := ((token.TokType == TokOther) &&
		((s == "+") ||
		(s == "-")  ||
		(s == "!")  ||
		(s == "~")  ||
		(s == "&")  ||
		(s == "~&") ||
		(s == "|")  ||
		(s == "~|") ||
		(s == "^")  ||
		(s == "^~") ||
		(s == "~^")))
	if isPrefixOp {
		var ast AstExpr
		ast.Expr0 = & (AstIde {LeafValue: token})
		GetToken (lexer)
		ast.Exprs = [] AST { ParseExprUnary (lexer) }
		return & ast
	} else {
		ast := ParseExprAppOrSelection (lexer)
		debugPrint (os.Stdout, "ParseExprUnary: => ", ast, "\n")
		return ast
	}
}

// ParseExprAppOrSelection parses an application, member selection, vector selection or bit selection
func ParseExprAppOrSelection (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseExprAppOrSelection ...", nil, "\n")

	ast := ParseExprPrimary (lexer)

	for {
		if TokenIsSpecificPunctuation (lexer, ".") {
			// Member select
			debugPrint (os.Stdout, "ParseExprAppOrSelection. MemberSelect ...", nil, "\n")

			var astExpr AstExpr
			astExpr.Expr0 = makeIdeFromToken (* lexer.Token, "PrimMemberSelect")

			GetToken (lexer)

			astExpr.Exprs = [] AST { ast, ParseVarIde (lexer) }
			ast = & astExpr

		} else if TokenIsSpecificPunctuation (lexer, "[") {
			// Vector or bit selection
			debugPrint (os.Stdout, "ParseExprAppOrSelection. [] Select ...", nil, "\n")

			var astExpr AstExpr
			tok := *lexer.Token

			GetToken (lexer)

			ast1 := ParseExpr (lexer)
			if TokenIsSpecificPunctuation (lexer, ":") {
				GetToken (lexer)
				ast2 := ParseExpr (lexer)
				astExpr.Expr0 = makeIdeFromToken (tok, "PrimBitSelect")
				astExpr.Exprs = [] AST { ast, ast1, ast2 }
			} else {
				astExpr.Expr0 = makeIdeFromToken (tok, "PrimIndex")
				astExpr.Exprs = [] AST { ast, ast1 }
			}
			skipPunctuationMust (lexer, "]")
			ast = & astExpr

		} else if TokenIsSpecificPunctuation (lexer, "(") {
			// Function call
			debugPrint (os.Stdout, "ParseExprAppOrSelection. [] App ...", nil, "\n")

			var astExpr AstExpr
			astExpr.Expr0 = makeIdeFromToken (* lexer.Token, "Apply")
			astExpr.Exprs = [] AST { ast }

			GetToken (lexer)
			for {
				if skipPunctuationOpt (lexer, ")") { break }
				astJ := ParseExpr (lexer)
				astExpr.Exprs = append (astExpr.Exprs, astJ)
				if ! TokenIsSpecificPunctuation (lexer, ")") {
					skipPunctuationMust (lexer, ",")
				}
			}
			ast = & astExpr

		} else if TokenIsSpecificPunctuation (lexer, "#") {
			// This is actually a type expression, a constructed type
			// We'll treat it as a value expr for now, convert it later
			debugPrint (os.Stdout, "ParseExprAppOrSelection. [] TypeApp ...", nil, "\n")

			var astExpr AstExpr
			astExpr.Expr0 = makeIdeFromToken (* lexer.Token, "Apply")
			astExpr.Exprs = [] AST { ast }
			GetToken (lexer)
			skipPunctuationMust (lexer, "(")

			for {
				if skipPunctuationOpt (lexer, ")") { break }
				astJ := ParseExpr (lexer)
				astExpr.Exprs = append (astExpr.Exprs, astJ)
				if ! TokenIsSpecificPunctuation (lexer, ")") {
					skipPunctuationMust (lexer, ",")
				}
			}
			ast = & astExpr

		} else {
			break
		}
	}

	debugPrint (os.Stdout, "ParseExprAppOrSelection: => ", ast, "\n")
	return ast
}

/* TODO: DELETE?
// ParseExprAppOrSelection parses a function application or selection
func ParseExprApp (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseExprApp ...", nil, "\n")
	var ast AST

	ast0 := ParseExprPrimary (lexer)

	if TokenIsSpecificPunctuation (lexer, "(") {
		// Function call
		var astExpr AstExpr
		astExpr.Expr0 = makeIdeFromToken (* lexer.Token, "Apply")
		astExpr.Exprs = [] AST { ast0 }

		GetToken (lexer)
		for {
			if TokenIsSpecificPunctuation (lexer, ")") { break }
			astJ := ParseExpr (lexer)
			astExpr.Exprs = append (astExpr.Exprs, astJ)
			if ! TokenIsSpecificPunctuation (lexer, ")") {
				skipPunctuationMust (lexer, ",")
			}
		}
		skipPunctuationMust (lexer, ")")
		ast = & astExpr

	} else if TokenIsSpecificPunctuation (lexer, "#") {
		// This is actually a type expression, a constructed type
		// We'll treat it as a value expr for now, convert it later
		var astExpr AstExpr
		astExpr.Expr0 = makeIdeFromToken (* lexer.Token, "Apply")
		astExpr.Exprs = [] AST { ast0 }
		GetToken (lexer)
		skipPunctuationMust (lexer, "(")

		for {
			if TokenIsSpecificPunctuation (lexer, ")") { break }
			astJ := ParseExpr (lexer)
			astExpr.Exprs = append (astExpr.Exprs, astJ)
			if ! TokenIsSpecificPunctuation (lexer, ")") {
				skipPunctuationMust (lexer, ",")
			}
		}
		skipPunctuationMust (lexer, ")")
		ast = & astExpr

	} else {
		ast = ast0
	}

	debugPrint (os.Stdout, "ParseExpr: => ", ast, "\n")
	return ast
}
*/

// ParseExprPrimary parses a basic, non-compound expression
func ParseExprPrimary (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseExprPrimary ...", nil, "\n")

	var result AST

	if lexer.Token.TokType == TokInteger {
		// Integer contant
		ast := AstNum {lexer.Token}
		GetToken (lexer)
		result = & ast

	} else if lexer.Token.TokType == TokString {
		// String
		ast := AstString {lexer.Token}
		GetToken (lexer)
		result = & ast

	} else if TokenIsKeyword (lexer, "action") {
		// actionBlock
		result = ParseBlock (lexer, "action")

	} else if TokenIsKeyword (lexer, "actionvalue") {
		// actionValueBlock
		result = ParseBlock (lexer, "actionvalue")

	} else if TokenIsKeyword (lexer, "begin") {
		// actionValueBlock
		result = ParseBlock (lexer, "begin")

	} else if skipKeywordOpt (lexer, "interface") {
		// interface expression
		result = ParseInterfaceExpr (lexer)

	} else if TokenIsKeyword (lexer, "if") {
		// case
		result = ParseIf (lexer)

	} else if TokenIsKeyword (lexer, "case") {
		// case
		result = ParseCase (lexer)

	} else if TokenIsKeyword (lexer, "tagged") {
		// tagged union expr
		GetToken (lexer)
		var ast AstTaggedUnionExpr
		ast.Name = ParseConstIde (lexer)
		// TODO: the following should check for any token that can't begin an Expr
		if ! TokenIsSpecificPunctuation (lexer, ";") {
			ast.Expr = ParseExpr (lexer)
		}
		result = & ast

	} else if lexer.Token.TokType == TokIde {
		// identifier
		astName := AstIde {LeafValue: lexer.Token}
		GetToken (lexer)
		if TokenIsSpecificPunctuation (lexer, "{") {
			// Struct expression
			GetToken (lexer)
			var astStruct AstStructExpr
			astStruct.Name = & astName
			for {
				if TokenIsSpecificPunctuation (lexer, "}") { break }
				astStruct.MemberNames = append (astStruct.MemberNames, ParseVarIde (lexer))
				skipPunctuationMust (lexer, ":")
				astStruct.MemberExprs = append (astStruct.MemberExprs, ParseExpr (lexer))
				if ! TokenIsSpecificPunctuation (lexer, "}") {
					skipPunctuationMust (lexer, ",")
				}
			}
			skipPunctuationMust (lexer, "}")
			result = & astStruct
		} else {
			result = & astName
		}

	} else if TokenIsSpecificPunctuation (lexer, "?") {
		// Wildcard ?
		ast := AstIde {LeafValue: lexer.Token}
		GetToken (lexer)
		result = & ast

	} else if TokenIsSpecificPunctuation (lexer, "{") {
		// Bit Concat { e1,...,eN}
		var ast AstExpr
		ast.Expr0 = makeIdeFromToken (* lexer.Token, "PrimBitConcat")

		GetToken (lexer)
		for {
			if TokenIsSpecificPunctuation (lexer, "}") { break }
			astJ := ParseExpr (lexer)
			ast.Exprs = append (ast.Exprs, astJ)
			if ! TokenIsSpecificPunctuation (lexer, "}") {
				skipPunctuationMust (lexer, ",")
			}
		}
		GetToken (lexer)

		result = & ast

	} else if TokenIsSpecificPunctuation (lexer, "(") {
		// parenthesized expr
		GetToken (lexer)
		ast := ParseExpr (lexer)
		skipPunctuationMust (lexer, ")")
		result = ast

	} else if skipKeywordOpt (lexer, "seq") {
		// StmtFSM seq..endseq
		result = ParseFSMseq (lexer)

	} else if skipKeywordOpt (lexer, "par") {
		// StmtFSM par..endpar
		result = ParseFSMpar (lexer)

	} else {
		raiseParseError (lexer, "Expecting a basic, non-compound expression\n")
		var ast AST
		result = ast
	}
	debugPrint (os.Stdout, "ParseExprPrimary: => ", result, "\n")
	return result
}

// ParseBlock parses: action ... endaction, actionvalue ... endactionvalue and begin ... end
// Current token is 'action'/'actionvalue'/'begin'
func ParseBlock (lexer *Lexer, kind string) (AST) {
	debugPrint (os.Stdout, "ParseBlock ...", nil, "\n")

	var ast AstBlock
	ast.BlockKind = kind

	// Skip 'action'/'actionvalue' keyword
	GetToken (lexer)
	
	// Get the optional block name
	if TokenIsSpecificPunctuation (lexer, ":") {
		GetToken (lexer)
		ast.BlockName = ParseVarIde (lexer)
	}

	ast.Stmts = ParseStmts (lexer)

	// Parse the 'endaction'/'endactionvalue' keyword
	endkeyword := MatchingEndKeyword (kind)
	if ! TokenIsKeyword (lexer, endkeyword) {
		raiseParseError (lexer, fmt.Sprintf ("Expecting '%s'\n", endkeyword))
	}
	GetToken (lexer)
	
	// Get the optional block name
	if TokenIsSpecificPunctuation (lexer, ":") {
		GetToken (lexer)
		end_blockName := ParseVarIde (lexer)
		// TODO: use SameIde predicate here, not !=
		if ast.BlockName != end_blockName {
			raiseParseError (lexer, "Mismatched block names after 'action' and 'endaction'\n")
		}
	}

	debugPrint (os.Stdout, "ParseBlock: => ", & ast, "\n")
	return & ast
}

// ParseIf parses: if (E) S1 [ else S2 ]
// Current token is 'if'
func ParseIf (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseIf ...", nil, "\n")

	var astIf AstIf

	// Skip 'if' keyword
	GetToken (lexer)
	skipPunctuationMust (lexer, "(")
	astIf.ExprCond = ParseExpr (lexer)
	skipPunctuationMust (lexer, ")")
	astIf.StmtThen = ParseStmt (lexer)

	astIf.StmtElse = nil
	if skipKeywordOpt (lexer, "else") {
		astIf.StmtElse = ParseStmt (lexer)
	}
	result := & astIf
	debugPrint (os.Stdout, "ParseIf: => ", result, "\n")
	return result
}

// ParseCase parses: case (...) ... endcase and case (...) matches ... endcase
// Current token is 'case'
func ParseCase (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseCase ...", nil, "\n")

	// Skip 'case' keyword
	GetToken (lexer)
	skipPunctuationMust (lexer, "(")
	expr := ParseExpr (lexer)
	skipPunctuationMust (lexer, ")")
	
	isPatternCase := TokenIsKeyword (lexer, "matches")
	if isPatternCase { GetToken (lexer) }

	var lhss [] AST
	var rhss [] AST

	for {
		if skipKeywordOpt (lexer, "endcase") { break }

		if TokenIsKeyword (lexer, "default") {
			lhs := makeIdeFromToken (* lexer.Token, "default")
			lhss = append (lhss, lhs)
			GetToken (lexer)
		} else if isPatternCase {
			lhss = append (lhss, ParsePattern (lexer))
		} else {
			lhss = append (lhss, ParseExpr (lexer))
		}
		skipPunctuationMust (lexer, ":")
		rhss  = append (rhss, ParseStmt (lexer))
	}

	var result AST
	if isPatternCase {
		result = & AstPatternCase {Expr: expr, Patterns: lhss, Exprs: rhss}
	} else {
		result = & AstCase {Expr: expr, Labels: lhss, Exprs: rhss}
	}

	debugPrint (os.Stdout, "ParseCase: => ", result, "\n")
	return result
}

// ParseFor parses: "for (init; cond; incr) body"
// Current token is 'for'
func ParseFor (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseFor ...", nil, "\n")

	var astFor AstFor

	// Skip 'for' keyword
	GetToken (lexer)
	skipPunctuationMust (lexer, "(")
	astFor.LoopInit = ParseStmt (lexer)
	astFor.LoopCond = ParseExpr (lexer)
	skipPunctuationMust (lexer, ";")
	astFor.LoopIncr = ParseStmt (lexer)
	skipPunctuationMust (lexer, ")")
	astFor.LoopBody = ParseStmt (lexer)

	result := & astFor
	debugPrint (os.Stdout, "ParseFor: => ", result, "\n")
	return result
}

// ParseWhile parses: "while (cond) body"
// Current token is 'while'
func ParseWhile (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseWhile ...", nil, "\n")

	var astWhile AstWhile

	// Skip 'while' keyword
	GetToken (lexer)
	skipPunctuationMust (lexer, "(")
	astWhile.LoopCond = ParseExpr (lexer)
	skipPunctuationMust (lexer, ")")
	astWhile.LoopBody = ParseStmt (lexer)

	result := & astWhile
	debugPrint (os.Stdout, "ParseWhile: => ", result, "\n")
	return result
}

// ParseInterfaceExpr parses: "interface ... endinterface"
// Current token is just past 'interface'
func ParseInterfaceExpr (lexer *Lexer) (* AstInterfaceExpr) {
	debugPrint (os.Stdout, "ParseInterfaceExpr ...", nil, "\n")

	var astIfc AstInterfaceExpr
	astIfc.Type = ParseConstIde (lexer)
	skipPunctuationMust (lexer, ";")

	astIfc.MethodAndIfcDefs = ParseStmts (lexer)

	skipKeywordMust (lexer, "endinterface")
	if skipPunctuationOpt (lexer, ":") {
		endide := ParseConstIde (lexer)
		if ! SameIde (astIfc.Type, endide) {
			raiseParseError (lexer, "Mismatched type names after 'interface' and 'endinterface'\n")
		}
	}

	result := & astIfc
	debugPrint (os.Stdout, "ParseInterfaceExpr: => ", result, "\n")
	return result
}

// ================================================================

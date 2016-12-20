// Copyright (c) 2016-2017 Rishiyur Nikhil and Bluespec, Inc.  All Rights Reserved.

// Part of goParseBSV, a parser for BSV files
// This part defines parsers for patterns.

package	goParseBSV

import (
	// golang packages
	"os"
 	"strconv"
)

// ================================================================
// Parsers

// ParsePattern is the top-level parser for patterns
func ParsePattern (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParsePattern ...", nil, "\n")

	var result AST

	if skipPunctuationOpt (lexer, "(") {
		// Parenthesized pattern
		result = ParsePattern (lexer)
		skipPunctuationMust (lexer, ")")

	} else if TokenIsSpecificPunctuation (lexer, ".") {
		// .* or .x (pattern variable)
		GetToken (lexer)
		if TokenIsSpecificPunctuation (lexer, "*") {
			astIde := AstIde { LeafValue: lexer.Token }
			GetToken (lexer)
			result = & AstPatternVarIde { varIde: & astIde }
		} else {
			ast := ParseVarIde (lexer)
			result = & AstPatternVarIde { varIde: ast }
		}

	} else if lexer.Token.TokType == TokInteger {
		// Integer contant pattern
		ast := AstNum {lexer.Token}
		GetToken (lexer)
		result = & AstPatternConst { constant: & ast }

	} else if lexer.Token.TokType == TokString {
		// String contant pattern
		ast := AstString {lexer.Token}
		GetToken (lexer)
		result = & AstPatternConst { constant: & ast }

	} else if TokenIsKeyword (lexer, "tagged") {
		// Tagged union or struct pattern
		debugPrint (os.Stdout, "ParsePattern/tagged ...", nil, "\n")
		GetToken (lexer)
		tag := ParseConstIde (lexer)
		if TokenIsSpecificPunctuation (lexer, "{") {
			// Struct pattern
			result = ParseStructPattern (lexer, tag)
		} else {
			// Tagged Union pattern
			result = ParseTaggedUnionPattern (lexer, tag)
		}

	} else if TokenIsSpecificPunctuation (lexer, "{") {
		// Tuple pattern
		result = ParseTuplePattern (lexer)

	} else if TokenIsIde (lexer) {
		// Ide contant pattern (enum label)
		ast := AstIde {LeafValue: lexer.Token}
		GetToken (lexer)
		result = & AstPatternConst { constant: & ast }

	} else {
		raiseParseError (lexer, "Expecting a pattern")
	}

	debugPrint (os.Stdout, "ParsePattern: => ", result, "\n")
	return result
}

// ParseStructPattern parses a struct pattern; current token is the opening '{'
func ParseStructPattern (lexer *Lexer, structName AST) (AST) {
	debugPrint (os.Stdout, "ParseStructPattern ...", nil, "\n")

	var ast AstStructPattern
	ast.StructName = structName
	debugPrint (os.Stdout, "ParseStructPattern: structName: ", structName, "\n");
	GetToken (lexer)
	for {
		if TokenIsSpecificPunctuation (lexer, "}") { break }
		memberName := ParseVarIde (lexer)
		debugPrint (os.Stdout, "ParseStructPattern: memberName: ", memberName, "\n")
		ast.MemberNames = append (ast.MemberNames, memberName)
		skipPunctuationMust (lexer, ":")
		pattern := ParsePattern (lexer)
		debugPrint (os.Stdout, "ParseStructPattern: pattern: ", pattern, "\n")
		ast.MemberPatterns = append (ast.MemberPatterns, pattern)
		if ! TokenIsSpecificPunctuation (lexer, "}") {
			skipPunctuationMust (lexer, ",")
		}
	}
	GetToken (lexer)
	return & ast
}

// ParseTuplePattern parses a tuple pattern from the opening '{'
// Tuples are treated as Struct patterns with struct name "{}"
// and member names tuple_1, tuple_2, ...
func ParseTuplePattern (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseTuplePattern ...", nil, "\n")

	var ast AstStructPattern
	var tok Token = * lexer.Token
	ast.StructName = makeIdeFromToken (tok, "{}")
	GetToken (lexer)
	var j int = 1
	for {
		if TokenIsSpecificPunctuation (lexer, "}") { break }
		memberName := makeIdeFromToken (tok, "tuple_" + 	strconv.Itoa (j))
		ast.MemberNames = append (ast.MemberNames, memberName)
		pattern := ParsePattern (lexer)
		ast.MemberPatterns = append (ast.MemberPatterns, pattern)
		if ! TokenIsSpecificPunctuation (lexer, "}") {
			skipPunctuationMust (lexer, ",")
		}
		j += 1
	}
	GetToken (lexer)
	return & ast
}

// ParseTaggedUnionPattern parses a tagged union pattern: 'tagged tagname pat'
// Current token is first pattern in pat
func ParseTaggedUnionPattern (lexer *Lexer, tag AST) (AST) {
	debugPrint (os.Stdout, "ParseTaggedUnionPattern ...", nil, "\n")

	var ast AstTaggedUnionPattern
	ast.TaggedUnionName = tag

	// Check if the next token starts a pattern
	b1 := TokenIsSpecificPunctuation (lexer, ".")       // var, wildcards
	b2 := lexer.Token.TokType == TokInteger             // const int
	b3 := lexer.Token.TokType == TokString              // const string
	b4 := TokenIsKeyword (lexer, "tagged")              // struct/tagged union
	b5 := TokenIsSpecificPunctuation (lexer, "{")       // tuple
	b6 := TokenIsIde (lexer)                            // const pattern Enum label
	b7 := TokenIsSpecificPunctuation (lexer, "(")       // parenthesized
	if b1 || b2 || b3 || b4 || b5 || b6 || b7 {
		ast.MemberPattern = ParsePattern (lexer)
	}
	return & ast
}

// ================================================================

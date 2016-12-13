// Part of goParseBSV, a parser for BSV files
// This part defines parsers for type definitions and type exprs.

package	goParseBSV

import (
	// golang packages
	// -- none --
)

// ================================================================
// AST parsers

// ParseTypeExpr parses a complete type expression
// in a typedef or var declaration or assignment.
// (typevar, typenum, constructed, struct)
func ParseTypeExpr (lexer *Lexer) (AST) {
	// Not supported: "bit [i:j]"

	if lexer.Token.TokType == TokInteger {
		// Numeric type
		ast := AstTypeNum {lexer.Token}
		GetToken (lexer)
		return & ast

	} else if TokenIsIde (lexer) || TokenIsKeyword (lexer, "module") {
		// Constructed type
		return ParseTypeConstructed (lexer)
	} else {
		raiseParseError (lexer, "Expecting a type expression\n")
		var ast AST
		return ast
	}
}

func ParseTypeConstructed (lexer *Lexer) (AST) {
	var ast AstTypeConstructed

	// Current token is the identifier for the type
	if TokenIsKeyword (lexer, "module") {
		ast.Constructor = makeIdeFromToken (*lexer.Token, "module")
		GetToken (lexer)
	} else {
		constructor := ParseIde (lexer)
		ast.Constructor = constructor
	}

	// Get type args, if any
	if skipPunctuationOpt (lexer, "#") {
		skipPunctuationMust (lexer, "(")
		for {
			argAst := ParseTypeExpr (lexer)
			ast.Args = append (ast.Args, argAst)
			commaFound := skipPunctuationOpt (lexer, ",")
			if ! commaFound {
				break
			}
		}
		skipPunctuationMust (lexer, ")")
	}
	return & ast
}

func ParseTypedefDefinedAsStruct (lexer *Lexer) (AST) {
	// Already consumed the "struct" keyword
	skipPunctuationMust (lexer, "{")
	ast := AstTypedefDefinedAsStruct {}
	for {
		rbraceFound := skipPunctuationOpt (lexer, "}")
		if rbraceFound { break };

		memberType := ParseTypeExpr (lexer)
		memberName := ParseVarIde (lexer)
		ast.StructMemberNames = append (ast.StructMemberNames, memberName)
		ast.StructMemberTypes = append (ast.StructMemberTypes, memberType)

		if ! TokenIsSpecificPunctuation (lexer, "}") {
			skipPunctuationMust (lexer, ";")
		}
	}
	return & ast
}

func ParseTypedefDefinedAsTaggedUnion (lexer *Lexer) (AST) {
	// Already consumed the "union tagged" keyword
	skipPunctuationMust (lexer, "{")
	ast := AstTypedefDefinedAsTaggedUnion {}
	for {
		rbraceFound := skipPunctuationOpt (lexer, "}")
		if rbraceFound { break };

		typeExpr := ParseTypeExpr (lexer)
		memberName := ParseVarIde (lexer)
		ast.TaggedUnionMemberNames = append (ast.TaggedUnionMemberNames, memberName)
		ast.TaggedUnionMemberTypes = append (ast.TaggedUnionMemberTypes, typeExpr)

		if ! TokenIsSpecificPunctuation (lexer, "}") {
			skipPunctuationMust (lexer, ";")
		}
	}
	return & ast
}

func ParseTypedefDefinedAsEnum (lexer *Lexer) (AST) {
	// Already consumed the "enum" keyword
	skipPunctuationMust (lexer, "{")
	ast := AstTypedefDefinedAsEnum {}
	for {
		if skipPunctuationOpt (lexer, "}") { break };

		typedefEnumElement := ParseConstIde (lexer)
		ast.TypedefEnumElements = append (ast.TypedefEnumElements, typedefEnumElement)
		var val *AstNum = nil
		if skipPunctuationOpt (lexer, "=") {

			if lexer.Token.TokType == TokInteger {
				// Integer contant
				val = & AstNum {lexer.Token}
				GetToken (lexer)
			} else {
				raiseParseError (lexer, "Expecting a literal integer as enum label value\n")
			}
		}
		ast.TypedefEnumVals = append (ast.TypedefEnumVals, val)

		if ! TokenIsSpecificPunctuation (lexer, "}") {
			skipPunctuationMust (lexer, ",")
		}
	}
	return & ast
}

// ParseTypedefDefinee parses the definee of a BSV typedef: "Constr #(typeformal,...,typeformal)"
// where each typeformal is "type typevar" or "numeric type typevar"
func ParseTypedefDefinee (lexer *Lexer) (*AstTypedefDefinee) {
	var ast AstTypedefDefinee

	ast.TypeConstructor = ParseConstIde (lexer)

	formalsFound := skipPunctuationOpt (lexer, "#")
	if formalsFound {
		skipPunctuationMust (lexer, "(")
		for {
			rparenFound := skipPunctuationOpt (lexer, ")")
			if (rparenFound) { break }

			numeric := skipKeywordOpt (lexer, "numeric")
			skipKeywordMust (lexer, "type")
			formal := ParseVarIde (lexer)
			ast.TypeFormals = append (ast.TypeFormals, formal)
			kind := KindValue
			if numeric { kind = KindNumeric }
			ast.TypeFormalKinds   = append (ast.TypeFormalKinds, kind)

			if ! TokenIsSpecificPunctuation (lexer, ")") {
				skipPunctuationMust (lexer, ",")
			}
		}
	}
	return & ast
}

// ================================================================

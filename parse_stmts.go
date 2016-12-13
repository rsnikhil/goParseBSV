// Part of goParseBSV, a parser for BSV files
// This part defines parsers for statements

package	goParseBSV

import (
	// golang packages
	"fmt"
	"os"
	"reflect"
)

// ParseStmts parses a sequence of statements
// Current token is the token after the opening 'begin/action/actionvalue/...' keyword
func ParseStmts (lexer *Lexer) ([] AST) {
	debugPrint (os.Stdout, "ParseStmts ...", nil, "\n")

	var stmts [] AST;

	for {
		if TokenIsBlockEndKeyword (lexer) { break }
		stmts = append (stmts, ParseStmt (lexer))
	}
	debugPrint (os.Stdout, "ParseStmts ==> ", nil, "\n")
	return stmts
}

// ParseStmt parses a statement
// Current token is the first token of the statement
func ParseStmt (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseStmt ... ", nil, "\n")

	var result AST

	if skipPunctuationOpt (lexer, "(*") {
		result = ParseAttrInstance (lexer)

	} else if TokenIsKeyword (lexer, "let") {
		var astLet AstLet
		GetToken (lexer)
		astLet.Ide = ParseVarIde (lexer)
		if TokenIsSpecificPunctuation (lexer, "=") {
			GetToken (lexer)
			astLet.Kind = BindingKindEq
		} else {
			skipPunctuationMust (lexer, "<-")
			astLet.Kind = BindingKindLArrow
		}
		astLet.Expr = ParseExpr (lexer)
		result = & astLet

	} else if TokenIsKeyword (lexer, "match") {
		var astMatch AstMatch
		GetToken (lexer)
		astMatch.Pattern = ParsePattern (lexer)
		skipPunctuationMust (lexer, "=")
		astMatch.Expr = ParseExpr (lexer)
		result = & astMatch

	} else if TokenIsKeyword (lexer, "function") {
		result = ParseFunctionDef (lexer)

	} else if TokenIsKeyword (lexer, "method") {
		result = ParseMethodDef (lexer)

	} else if TokenIsKeyword (lexer, "interface") {
		result = ParseInterfaceDef (lexer)

	} else if TokenIsKeyword (lexer, "module") {
		result = ParseModuleDef (lexer)

	} else if TokenIsKeyword (lexer, "rule") {
		result = ParseRule (lexer)

	} else if TokenIsKeyword (lexer, "if") {
		result = ParseIf (lexer)

	} else if TokenIsKeyword (lexer, "for") {
		result = ParseFor (lexer)

	} else if TokenIsKeyword (lexer, "while") {
		result = ParseWhile (lexer)

	} else {
		result = ParseVarDeclOrAssign (lexer)
	}
	skipPunctuationOpt (lexer, ";")

	debugPrint (os.Stdout, "ParseStmt: => ", result, "\n")
	return result
}

// Strategy for ParseVarDeclOrAssign:

// Statements include 'e', 'type lhs=rhs' and 'lhs=rhs'. So, we don't
//     know initially whether we're parsing an expression, a type, or
//     an lhs.  Thus, we first parse an expression.  If the next token
//     is an identifier, the expression we just parsed is actually a
//     type and the next token starts an lhs.

// ParseVarDeclOrAssign parses
// variable declarations with '=' initializers: type varOrArray = e, varOrArray=e, ...,
// variable declarations with '<-' assignment:  type var <- e
// variable = assignments: lhs = e
// variable '<-' assignments: x <- e
func ParseVarDeclOrAssign (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseVarDeclOrAssign ... ", nil, "\n")

	var result AST

	ast := ParseExpr (lexer)    // may be a type, actually

	if skipPunctuationOpt (lexer, "=") {
		// '=' assignment of a previously declared variable
		// TODO: check that ast is a legal lvalue
		var astAssign AstAssign
		astAssign.Lhs = ast
		astAssign.Kind = BindingKindEq
		astAssign.Rhs = ParseExpr (lexer)
		result = & astAssign

	} else if skipPunctuationOpt (lexer, "<-") {
		// '<-' assignment of a previously declared variable
		// TODO: check that ast is a varIde
		var astAssign AstAssign
		astAssign.Lhs = ast
		astAssign.Kind = BindingKindLArrow
		astAssign.Rhs = ParseExpr (lexer)
		result = & astAssign

	} else if (! canBeTypeExpr (ast)) {
		result = ast

	} else if TokenIsIde (lexer) {
		astType := convertExprToTypeExpr (lexer, ast)
		result = ParseVarDecl (lexer, astType)

	} else {
		result = ast
	}

	debugPrint (os.Stdout, "ParseVarDeclOrAssign: => ", result, "\n")
	return result
}

// ParseVarDecl parses variable-declaration statement: type varInit, varInit, ... ;
// variable declarations with '=' initializers: type varOrArray = e, varOrArray=e, ...,
// variable declarations with '<-' assignment:  type var <- e
// Current token is first lhs
func ParseVarDecl (lexer *Lexer, astType AST) (AST) {
	debugPrint (os.Stdout, "ParseVarDecl ... ", nil, "\n")

	var ast AstVarDecl

	ast.Type = astType
	debugPrint (os.Stdout, "ParseVarDecl Type: ", ast.Type, "\n")

	// Get varInit, ..., varInit ;
	for {
		if TokenIsSpecificPunctuation (lexer, ";") { break }
		varInit := ParseVarInit (lexer)
		ast.VarInits = append (ast.VarInits, varInit)
		if ! TokenIsSpecificPunctuation (lexer, ";") {
			skipPunctuationMust (lexer, ",")
		}
	}

	debugPrint (os.Stdout, "ParseVarDecl: => ", & ast, "\n")
	return & ast
}

// ParseVarInit parses an assignment 'Ide [e]...[e] = Expr;'
func ParseVarInit (lexer *Lexer) (*AstVarInit) {
	debugPrint (os.Stdout, "ParseVarInit ... ", nil, "\n")

	var astVarInit AstVarInit

	// Get identifier being defined
	astVarInit.Ide     = ParseVarIde (lexer)
	debugPrint (os.Stdout, "ParseVarInit Ide: ", astVarInit.Ide, "\n")

	// Get array dims, if any
	for skipPunctuationOpt (lexer, "[") {
		arrayDim := ParseExpr (lexer)
		skipPunctuationMust (lexer, "]")
		astVarInit.ArrayDims = append (astVarInit.ArrayDims, arrayDim)
		debugPrint (os.Stdout, "ParseVarInit arrayDim: ", arrayDim, "\n")
	}

	// Get initializer, if any
	if skipPunctuationOpt (lexer, "=") {
		astVarInit.Kind = BindingKindEq
		astVarInit.Init = ParseExpr (lexer)
		debugPrint (os.Stdout, "ParseVarInit = Init:", astVarInit.Init, "\n")

	} else if skipPunctuationOpt (lexer, "<-") {
		astVarInit.Kind = BindingKindLArrow
		astVarInit.Init = ParseExpr (lexer)
		debugPrint (os.Stdout, "ParseVarInit <- Init:", astVarInit.Init, "\n")
	} else {
		astVarInit.Kind = BindingKindNone
	}

	debugPrint (os.Stdout, "ParseVarInit: => ", & astVarInit, "\n")
	return & astVarInit
}

// ParseRule parses 'rule ... endrule'
// Current token is 'rule' keyword
func ParseRule (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseRule ... ", nil, "\n")

	var astRule AstRule

	// Skip past 'rule' keyword
	GetToken (lexer)

	// Get the rule name
	astRule.Name = ParseIde (lexer)

	if skipPunctuationOpt (lexer, "(") {
		astRule.Cond = ParseExpr (lexer)
		skipPunctuationMust (lexer, ")")
	}
	skipPunctuationMust (lexer, ";")

	// Get rule body
	astRule.Stmts = ParseStmts (lexer)

	// Parse the 'endrule' keyword
	skipKeywordMust (lexer, "endrule")

	// TODO: optional repeat of rulename after endrule

	result := & astRule
	debugPrint (os.Stdout, "ParseRule: => ", result, "\n")
	return result
}

// ParseFunctionProto parses 'function type name (type formal, ..., type formal)'
// Current token is 'function' keyword
func parseFunctionProto (lexer *Lexer) (*AstFunctionProto) {
	debugPrint (os.Stdout, "ParseFunctionProto ... ", nil, "\n")

	var astFunctionProto AstFunctionProto

	// Skip past 'function' keyword
	GetToken (lexer)

	astFunctionProto.ResultType = ParseTypeExpr (lexer)
	debugPrint (os.Stdout, "ParseFunctionProto ResultType: ", astFunctionProto.ResultType, "\n")

	astFunctionProto.Name       = ParseVarIde (lexer)
	debugPrint (os.Stdout, "ParseFunctionProto Name ", astFunctionProto.Name, "\n")

	if skipPunctuationOpt (lexer, "(") {
		for {
			if skipPunctuationOpt (lexer, ")") { break }
			astFunctionProto.FormalTypes = append (astFunctionProto.FormalTypes, ParseTypeExpr (lexer))
			astFunctionProto.Formals     = append (astFunctionProto.Formals,     ParseVarIde (lexer))
			if ! TokenIsSpecificPunctuation (lexer, ")") {
				skipPunctuationMust (lexer, ",")
			}
		}
	}

	return & astFunctionProto
}

// ParseFunctionDef parses 'function ... endfunction' or 'function ... = e'
// Current token is 'function' keyword
func ParseFunctionDef (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseFunctionDef ... ", nil, "\n")

	var astFuncDef AstFunctionDef

	// Parse the function prototype
	astFuncDef.Proto = parseFunctionProto (lexer)

	// Get optional provisos
	if TokenIsKeyword (lexer, "provisos") {
		astFuncDef.Provisos = ParseProvisos (lexer)
	}

	if TokenIsSpecificPunctuation (lexer, "=") {
		// Body is assigned
		GetToken (lexer)
		astFuncDef.Body = ParseExpr (lexer)

	} else {
		// Body is a block
		skipPunctuationMust (lexer, ";")
		var stmts [] AST = ParseStmts (lexer)
		// Parse the 'endfunction' keyword
		if ! TokenIsKeyword (lexer, "endfunction") {
			raiseParseError (lexer, "Expecting 'endfunction'\n")
		}
		GetToken (lexer)
		astFuncDef.Body = & AstBlock {BlockKind: "begin", BlockName: nil, Stmts: stmts}
	}

	debugPrint (os.Stdout, "ParseFunctionDef: => ", & astFuncDef, "\n")
	return & astFuncDef
}

// ParseMethodDef parses 'method ... endmethod' or 'method ... = e'
// Current token is 'method' keyword
func ParseMethodDef (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseMethodDef ... ", nil, "\n")

	var astMethodDef AstMethodDef

	// Skip past 'method' keyword
	GetToken (lexer)

	astMethodDef.ResultType = ParseTypeExpr (lexer)
	debugPrint (os.Stdout, "ParseMethodDef ResultType: ", astMethodDef.ResultType, "\n")

	astMethodDef.Name       = ParseVarIde (lexer)
	debugPrint (os.Stdout, "ParseMethodDef Name ", astMethodDef.Name, "\n")

	// Get optional formal params
	if skipPunctuationOpt (lexer, "(") {
		for {
			if skipPunctuationOpt (lexer, ")") { break }
			astMethodDef.FormalTypes = append (astMethodDef.FormalTypes, ParseTypeExpr (lexer))
			astMethodDef.Formals     = append (astMethodDef.Formals,     ParseVarIde (lexer))
			if ! TokenIsSpecificPunctuation (lexer, ")") {
				skipPunctuationMust (lexer, ",")
			}
		}
	}

	// Get optional method condition
	if skipKeywordOpt (lexer, "if") {
		astMethodDef.Cond = ParseExpr (lexer)
	}

	if skipPunctuationOpt (lexer, "=") {
		astMethodDef.Body = ParseExpr (lexer)
	} else {
		skipPunctuationMust (lexer, ";")
		var stmts [] AST = ParseStmts (lexer)
		// Parse the 'endmethod' keyword
		if ! TokenIsKeyword (lexer, "endmethod") {
			raiseParseError (lexer, "Expecting 'endmethod'\n")
		}
		GetToken (lexer)
		astMethodDef.Body = & AstBlock {BlockKind: "begin", BlockName: nil, Stmts: stmts}
	}

	debugPrint (os.Stdout, "ParseMethodDef: => ", & astMethodDef, "\n")
	return & astMethodDef
}

// ParseInterfaceDef parses 'interface ... endinterface' or 'interface ... = e'
// Current token is 'interface' keyword
func ParseInterfaceDef (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseInterfaceDef ... ", nil, "\n")

	var result AST

	// Skip past 'interface' keyword
	GetToken (lexer)

	var typ AST = nil
	var name *AstIde = nil
	// Get the optional type of the interface being defined
	e := ParseExpr (lexer)

	if TokenIsIde (lexer) {
		// e was the type; convert it into a type expr
		typ = convertExprToTypeExpr (lexer, e)
		// Get the interface name
		name = ParseVarIde (lexer)
	} else {
		// e was the identifier
		ide, ok := e.(*AstIde)
		if ! ok {
			raiseParseError (lexer, "Expecting interface name\n")
		}
		name = ide
	}

	if skipPunctuationOpt (lexer, "=") {
		// Interface assign
		var aia AstInterfaceAssign
		aia.Type = typ
		aia.Name = name
		aia.Val  = ParseExpr (lexer)
		result = & aia

	} else {
		// Interface def
		skipPunctuationMust (lexer, ";")

		var aid AstInterfaceDef
		aid.Type = typ
		aid.Name = name
		aid.MethodAndIfcDefs = ParseStmts (lexer)
		skipKeywordMust (lexer, "endinterface")
		result = & aid
	}

	debugPrint (os.Stdout, "ParseInterfaceDef: => ", result, "\n")
	return result
}

// ParseModuleDef parses 'module ... endmodule'
// Current token is 'module' keyword
func ParseModuleDef (lexer *Lexer) (*AstModuleDef) {
	debugPrint (os.Stdout, "ParseModuleDef ... ", nil, "\n")

	var astModDef AstModuleDef

	// Skip past 'module' keyword
	GetToken (lexer)

	// Get the optional module type
	if skipPunctuationOpt (lexer, "[") {
		astModDef.ModuleType = ParseTypeExpr (lexer)
		debugPrint (os.Stdout, "ParseModuleDef ModuleType: ", astModDef.ModuleType, "\n")
		skipPunctuationMust (lexer, "]")
	}

	// Get the module name
	astModDef.Name = ParseVarIde (lexer)
	debugPrint (os.Stdout, "ParseModuleDef Name ", astModDef.Name, "\n")

	// Get optional params
	if skipPunctuationOpt (lexer, "#") {
		skipPunctuationMust (lexer, "(")
		for {
			if skipPunctuationOpt (lexer, ")") { break }
			astModDef.FormalParamTypes = append (astModDef.FormalParamTypes, ParseTypeExpr (lexer))
			astModDef.FormalParams     = append (astModDef.FormalParams,     ParseVarIde (lexer))
			if ! TokenIsSpecificPunctuation (lexer, ")") {
				skipPunctuationMust (lexer, ",")
			}
		}
	}

	// Get the interface type
	skipPunctuationMust (lexer, "(")
	astModDef.IfcType = ParseTypeExpr (lexer)
	skipPunctuationMust (lexer, ")")

	// Get optional provisos
	if TokenIsKeyword (lexer, "provisos") {
		astModDef.Provisos = ParseProvisos (lexer)
	}

	skipPunctuationMust (lexer, ";")

	// Get the module statements
	astModDef.Stmts = ParseStmts (lexer)

	// Parse the 'endmodule' keyword
	skipKeywordMust (lexer, "endmodule")

	if skipPunctuationOpt (lexer, ":") {
		endide := ParseVarIde (lexer)
		if ! SameIde (astModDef.Name, endide) {
			raiseParseError (lexer, "Mismatched module names after 'module' and 'endmodule'\n")
		}
		
	}

	result := & astModDef
	debugPrint (os.Stdout, "ParseModuleDef: => ", result, "\n")
	return result
}

// Parse 'provisos (...)'
// Current token is 'provisos'
func ParseProvisos (lexer *Lexer) ([] AST) {
	var result [] AST
	GetToken (lexer)
	skipPunctuationMust (lexer, "(")
	for {
		if TokenIsSpecificPunctuation (lexer, ")") { break }
		result = append (result, ParseTypeExpr (lexer))
		if ! TokenIsSpecificPunctuation (lexer, ")") {
			skipPunctuationMust (lexer, ",")
		}
	}
	GetToken (lexer)
	return result
}

// ================================================================
// Conversion of an Expr into a TypeExpr

// Predicate to check whether an expr can be a type expr

func canBeTypeExpr (ast AST) (bool) {
	debugPrint (os.Stdout, "canBeTypeExpr: ", ast, "\n")

	var result bool

	switch x := ast.(type) {

	case *AstNum:
		result = true

	case *AstIde:
		result = true

	case *AstExpr:
		// Check that this is an application with at least one component
		if (! AST_isSpecificIde (x.Expr0, "Apply")) {
			// Not an application
			result = false

		} else if len (x.Exprs) < 1 {
			// x.Exprs does not contain even 1 element (the type constructor)
			result = false

		} else if ! AST_isConstIde (x.Exprs [0]) {
			// x.Exprs [0] is not a Type Constructor
			result = false

		} else {
			// Check that the remaining components can be type exprs
			result = true
			for _, ast_e := range (x.Exprs [1:]) {
				result = result && canBeTypeExpr (ast_e)
			}
		}

	default:
		result = false
	}

	if debug {
		fmt.Fprintf (os.Stdout, "canBeTypeExpr: => %v\n", result)
	}
	return result
}

// Converts the arg, which is an Expr AST into a TypeExpr AST
func convertExprToTypeExpr (lexer *Lexer, ast AST) (AST) {
	debugPrint (os.Stdout, "convertExprToTypeExpr: ", ast, "\n")

	var result AST
	switch x := ast.(type) {

	case *AstNum:
		result = & AstTypeNum { LeafValue: x.LeafValue }

	case *AstIde:
		result = & AstTypeVar { LeafValue: x.LeafValue }

	case *AstExpr:
		err := false

		// Check that this is an application with at least one component
		if (! AST_isSpecificIde (x.Expr0, "Apply")) {
			err = true

		} else if len (x.Exprs) < 1 {
			// x.Exprs does not contain even 1 element (the type constructor)
			err = true
		}

		if err {
			fmt.Fprintf (os.Stderr, "ERROR: ConverExprToTypeExpr: illegal type expression\n")
			AST_pp (os.Stderr, "", ast)
			fmt.Fprintf (os.Stderr, "\n")
			fmt.Fprintf (os.Stderr, "    Expecting a type constructor applied to type args\n")
			raiseParseError (lexer, "Not a valid type expression\n")

		} else {
			// Check that the type constructor is an Identifier
			if ! AST_isConstIde (x.Exprs [0]) {
				fmt.Fprintf (os.Stderr, "ERROR: ConverExprToTypeExpr: illegal type expression\n")
				AST_pp (os.Stderr, "", ast)
				fmt.Fprintf (os.Stderr, "\n")
				fmt.Fprintf (os.Stderr, "    Expecting a type constructor\n")
				raiseParseError (lexer, "Not a valid type expression\n")

			} else {
				tycon, _ := (x.Exprs [0]).(*AstIde)

				ast_ts := make ([] AST, 0, len (x.Exprs) - 1)
				for _, ast_e := range (x.Exprs [1:]) {
					ast_ts = append (ast_ts, convertExprToTypeExpr (lexer, ast_e))
				}
				result = & AstTypeConstructed {Constructor: tycon, Args: ast_ts}
			}
		}

	default:
		fmt.Fprintf (os.Stderr, "ERROR: ConverExprToTypeExpr: illegal type expression\n")
		AST_pp (os.Stderr, "", ast)
		fmt.Fprintf (os.Stderr, "    ast type is: %v\n", reflect.TypeOf (ast))
		raiseParseError (lexer, "Not a type expression\n")
	}

	debugPrint (os.Stdout, "convertExprToTypeExpr: => ", result, "\n")
	return result
}

// ================================================================

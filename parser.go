// goParseBSV is a parser for BSV files.
// This is the top-level file of the parser.

package	goParseBSV

import (
	// golang packages
	"fmt"
	"os"
	"strings"
	"utils"
)

// ================================================================
// For debugging the parser itself.

var debug bool = false

func debugPrint (fout *os.File, pre string, ast AST, post string) () {
	if debug {
		fmt.Fprintf (fout, "%s", pre)
		if (ast != nil) { AST_pp (fout, "", ast) }
		fmt.Fprintf (fout, "%s", post)
	}
}

// ----------------------------------------------------------------

func raiseParseError (lexer *Lexer, msg string) {
	fmt.Fprintf (os.Stderr, "ERROR: %s", msg)
	lexer.PrintToken (os.Stderr, "    ")
	os.Exit (1)
}

// ================================================================
// Useful predicates on tokens and identifier ASTs

func TokenIsSpecificPunctuation (lexer *Lexer, s string) (bool) {
	token := lexer.Token
	if token.TokType != TokOther { return false }
	if token.StringVal != s { return false }
	return true
}

func TokenIsSpecificString (lexer *Lexer, s string) (bool) {
	token := lexer.Token
	if token.TokType != TokString { return false }
	if token.StringVal != s { return false }
	return true
}

func TokenIsIde (lexer *Lexer) (bool) {
	token := lexer.Token
	if token.TokType != TokIde { return false }
	return true
}

// tokenIsVarIde checks for BSV variable identifiers, i.e., first letter is lowercase
func tokenIsVarIde (token *Token) (bool) {
	return ((token != nil) &&
		(token.TokType == TokIde) &&
		(utils.ByteIsLower (token.StringVal [0]) || (token.StringVal [0] == '_')))
}

// tokenIsConstIde checks for BSV constant identifiers, i.e., first letter is uppercase
func tokenIsConstIde (token *Token) (bool) {
	return ((token != nil) &&
		(token.TokType == TokIde) &&
		(utils.ByteIsUpper (token.StringVal [0])))
}

func TokenIsKeyword (lexer *Lexer, s string) (bool) {
	token := lexer.Token
	if token.TokType != TokKeyword { return false }
	if token.StringVal != s { return false }
	return true
}

func TokenIsBlockEndKeyword (lexer *Lexer) (bool) {
	tok := lexer.Token
	s := tok.StringVal
	b1 := (tok.TokType == TokKeyword)
	b2 := ( (s == "end") ||
		(s == "endfunction") ||
		(s == "endrule") ||
		(s == "endmethod") ||
		(s == "endinterface") ||
		(s == "endmodule") ||
		(s == "endaction") ||
		(s == "endactionvalue"))
	return b1 && b2
}

func SameIde (ide1 *AstIde, ide2 *AstIde) (bool) {
	tok1 := ide1.LeafValue
	tok2 := ide2.LeafValue
	return (tok1.TokType == TokIde) &&
		(tok2.TokType == TokIde) &&
		(tok1.StringVal == tok2.StringVal)
}

func AST_isVarIde (ast AST) (bool) {
	astIde, ok := ast.(*AstIde)
	return ok && tokenIsVarIde (astIde.LeafValue)
}

func AST_isConstIde (ast AST) (bool) {
	astIde, ok := ast.(*AstIde)
	return ok && tokenIsConstIde (astIde.LeafValue)
}

func AST_isSpecificIde (ast AST, s string) (bool) {
	astIde, ok := ast.(*AstIde)
	return (ok &&
		(astIde.LeafValue != nil) &&
		(astIde.LeafValue.TokType == TokIde) &&
		(astIde.LeafValue.StringVal == s))
}

// ================================================================
// Consuming punctuation and keywords
// Tokens like { } < > [ ] ( ) ; : # , .

// skipPunctuationOpt skips an optional bit of punctuation "s"
// If found, consume, and return true
// else don't consume, and return false
func skipPunctuationOpt (lexer *Lexer, s string) (bool) {
	tok := lexer.Token
	if (tok.TokType == TokOther) && (tok.StringVal == s) {
		GetToken (lexer)
		return true
	} else {
		return false
	}
}

// skipPunctuationMust skips a necessary bit of punctuation "s"
func skipPunctuationMust (lexer *Lexer, s string) () {
	tok := lexer.Token
	if (tok.TokType == TokOther) && (tok.StringVal == s) {
		GetToken (lexer)
	} else {
		raiseParseError (lexer, fmt.Sprintf ("expecting %q\n", s))
	}
}

// skipKeywordOpt skips a necessary identifier.
// If found, consume, and return true
// else don't consume, and return false
func skipKeywordOpt (lexer *Lexer, s string) (bool) {
	tok := lexer.Token
	if (tok.TokType == TokKeyword) && (tok.StringVal == s) {
		GetToken (lexer)
		return true
	} else {
		return false
	}
}

// skipKeywordMust skips a necessary keyword
func skipKeywordMust (lexer *Lexer, s string) () {
	tok := lexer.Token
	if (tok.TokType == TokKeyword) && (tok.StringVal == s) {
		GetToken (lexer)
	} else {
		raiseParseError (lexer, fmt.Sprintf ("expecting keyword %q\n", s))
	}
}

// ================================================================
// Ides

func assertIsIde (lexer *Lexer) () {
	if (lexer.Token.TokType != TokIde) {
		raiseParseError (lexer, "Expecting an identifier\n")
	}
}

// Make an identifier with the given string from the given token
func makeIdeFromToken (tok Token, s string) (*AstIde) {
	tok.TokType   = TokIde
	tok.StringVal = s
	ast := AstIde {LeafValue: & tok}
	return & ast
}

// ParseIde parses and returns an identifier
func ParseIde (lexer *Lexer) (*AstIde) {
	assertIsIde (lexer)
	ast := AstIde {LeafValue: lexer.Token}
	GetToken (lexer)
	return & ast
}

// ParseVarIde parses and returns an identifier that is a varIde (lowercase first letter)
func ParseVarIde (lexer *Lexer) (*AstIde) {
	if tokenIsVarIde (lexer.Token) {
		ast := AstIde {LeafValue: lexer.Token}
		GetToken (lexer)
		return & ast
	} else {
		raiseParseError (lexer, "Expecting a var identifier (first letter lowercase)\n")
		return nil
	}
}

// ParseConstIde parses and returns an identifier that is a varIde (lowercase first letter)
func ParseConstIde (lexer *Lexer) (*AstIde) {
	if tokenIsConstIde (lexer.Token) {
		ast := AstIde {LeafValue: lexer.Token}
		GetToken (lexer)
		return & ast
	} else {
		raiseParseError (lexer, "Expecting a const identifier (first letter uppercase)\n")
		return nil
	}
}

// ================================================================
// Parse all top-level decls in a package

// ParsePackage is the top-level function to parse a BSV source file.
func ParsePackage (lexer *Lexer) (AST) {

	var ast AstPackage

	if skipKeywordOpt (lexer, "package") {
		ast.PackageName = ParseIde (lexer)
		skipPunctuationMust (lexer, ";")
	}

	for {
		if (lexer.Token.TokType == TokEof) && (ast.PackageName == nil) {
			break

		} else if skipPunctuationOpt (lexer, ";") {

		} else if skipPunctuationOpt (lexer, "(*") {
			attri := ParseAttrInstance (lexer)
			ast.PackageStmts = append (ast.PackageStmts, attri)
			continue

		} else if skipKeywordOpt (lexer, "import") {
			importStmt := ParseImport (lexer)
			ast.PackageStmts = append (ast.PackageStmts, importStmt)
			continue

		} else if skipKeywordOpt (lexer, "export") {
			exportStmt := ParseExport (lexer)
			ast.PackageStmts = append (ast.PackageStmts, exportStmt)
			continue

		} else if skipKeywordOpt (lexer, "interface") {
			decl := ParseInterfaceDecl (lexer)
			ast.PackageStmts = append (ast.PackageStmts, decl)
			continue

		} else if skipKeywordOpt (lexer, "typedef") {
			decl := ParseTypedef (lexer)
			ast.PackageStmts = append (ast.PackageStmts, decl)
			continue

		} else if TokenIsKeyword (lexer, "function") {
			decl := ParseFunctionDef (lexer)
			ast.PackageStmts = append (ast.PackageStmts, decl)
			continue

		} else if TokenIsKeyword (lexer, "instance") {
			decl := ParseInstance (lexer)
			ast.PackageStmts = append (ast.PackageStmts, decl)
			continue

		} else if TokenIsKeyword (lexer, "module") {
			decl := ParseModuleDef (lexer)
			ast.PackageStmts = append (ast.PackageStmts, decl)
			continue

		} else if skipKeywordOpt (lexer, "endpackage") {
			isColon := skipPunctuationOpt (lexer, ":")
			if isColon {
				endPkgName := ParseIde (lexer)
				if ! SameIde (ast.PackageName, endPkgName) {
					fmt.Fprintf (os.Stderr, "WARNING: 'package ")
					AST_pp (os.Stderr, "", ast.PackageName)
					fmt.Fprintf (os.Stderr, "' does not match 'endpackage:")
					AST_pp (os.Stderr, "", endPkgName)
					fmt.Fprintf (os.Stderr, "'    (ignoring this)\n")
					printLocation (os.Stdout, lexer, "  ")
				}
			}
			break
		} else {
			decl := ParseVarDeclOrAssign (lexer)
			ast.PackageStmts = append (ast.PackageStmts, decl)
			continue
		}
		/*
		} else {
			fmt.Fprintf (os.Stdout, "Stopping at:\n")
			printLocation (os.Stdout, lexer, "  ")
			raiseParseError (lexer, "Unrecognized input token")
			break
		}
                */
	}
	return & ast
}

// ================================================================
// Parse an import statement

// ParseImport parses an import line
// E.g., "import packagename :: *;"
func ParseImport (lexer *Lexer) (AST) {

	// "import" keyword already consumed

	if ! TokenIsSpecificString (lexer, "BDPI") {
		var astImport AstImport

		astImport.PackageName = ParseIde (lexer)
		skipPunctuationMust (lexer, "::")
		skipPunctuationMust (lexer, "*")
		skipPunctuationMust (lexer, ";")
		return & astImport

	} else {
		GetToken (lexer)
		var astImportBDPI AstImportBDPI
		astImportBDPI.Proto = parseFunctionProto (lexer)
		return & astImportBDPI
	}
}

// ================================================================
// Parse an export statement

// ParseExport parses an export line
// E.g., "export x, y(..), Z;"
// Current token is just after "export" keyword
func ParseExport (lexer *Lexer) (AST) {

	var astExport AstExport

	for {
		if skipPunctuationOpt (lexer, ";") { break }
		ide := ParseIde (lexer)
		withMembers := false
		if skipPunctuationOpt (lexer, "(") {
			skipPunctuationMust (lexer, "..")
			skipPunctuationMust (lexer, ")")
			withMembers = true
		}
		astExport.Ides = append (astExport.Ides, ide)
		astExport.WithMembers = append (astExport.WithMembers, withMembers)
		if ! TokenIsSpecificPunctuation (lexer, ";") {
			skipPunctuationMust (lexer, ",")
		}
	}
	result := & astExport
	debugPrint (os.Stdout, "ParseExport: => ", result, "\n")
	return result
}

// ================================================================
// Parse an attribute instance

// ParseAttrInstance parses (* ide, ide=expr, ide, ... *)
// Current token is just after (* opening token
func ParseAttrInstance (lexer *Lexer) (AST) {

	var attr AstAttrInstance

	for {
		if skipPunctuationOpt (lexer, "*)") { break }
		ide := ParseIde (lexer)
		var val AST = nil
		if skipPunctuationOpt (lexer, "=") {
			val = ParseExpr (lexer)
		}
		attr.Ides = append (attr.Ides, ide)
		attr.Vals = append (attr.Vals, val)
		if ! TokenIsSpecificPunctuation (lexer, "*)") {
			skipPunctuationMust (lexer, ",")
		}
	}
	result := & attr
	debugPrint (os.Stdout, "ParseAttrInstance: => ", result, "\n")
	return result
}

// ================================================================
// typedef statements
// e.g., "typedef typeDefinedAs newtype deriving (typeclass, ...);"

// ParseTypedef parses a BSV statement: 'typedef typeDefinedAs newtype deriving (typeclass, ...);'
func ParseTypedef (lexer *Lexer) (result *AstTypedef) {
	// "typedef" keyword already consumed.
	var ast AstTypedef

	// Parse the definedAs (enum, struct, union tagged, or existing type)
	if skipKeywordOpt (lexer, "enum") {
		ast.TypedefDefinedAs = ParseTypedefDefinedAsEnum (lexer)
	} else if skipKeywordOpt (lexer, "struct") {
		ast.TypedefDefinedAs = ParseTypedefDefinedAsStruct (lexer)
	} else if skipKeywordOpt (lexer, "union") {
		skipKeywordMust (lexer, "tagged")
		ast.TypedefDefinedAs = ParseTypedefDefinedAsTaggedUnion (lexer)
	} else {
		ast.TypedefDefinedAs = ParseTypeExpr (lexer)
	}

	// Parse the definee (the type being defined)
	ast.TypedefDefinee  = ParseTypedefDefinee (lexer)

	// Parse "deriving(..)" clause
	derivingFound   := skipKeywordOpt (lexer, "deriving")
	if derivingFound {
		skipPunctuationMust (lexer, "(");
		for {
			rparenFound := skipPunctuationOpt (lexer, ")");
			if rparenFound { break }
			deriveType := ParseIde (lexer);
			ast.TypeclassIdes = append (ast.TypeclassIdes, deriveType)
			if ! TokenIsSpecificPunctuation (lexer, ")") {
				skipPunctuationMust (lexer, ",")
			}
		}
	}
	skipPunctuationMust (lexer, ";")
	result = & ast
	return result
}

// ================================================================
// Interface declarations

// ParseInterfaceDecl parses BSV declaration: 'interface ... endinterface'
// Precondition: "interface" keyword already consumed
func ParseInterfaceDecl (lexer *Lexer) (*AstIfcDecl) {
	ifc := ParseTypedefDefinee (lexer)
	ast := AstIfcDecl {Ifc: ifc}

	skipPunctuationMust (lexer, ";")

	for {
		if skipKeywordOpt (lexer, "endinterface") { break }

		if skipPunctuationOpt (lexer, "(*") {
			ast.SubIfcOrMethodDecls = append (ast.SubIfcOrMethodDecls, ParseAttrInstance (lexer))

		} else if skipKeywordOpt (lexer, "interface") {
			// Parse a sub-interface decl: starts with "interface"
			var x AstIfcDeclSubIfcDecl
			x.SubIfcType = ParseTypeExpr (lexer)
			x.SubIfcName = ParseIde (lexer)
			ast.SubIfcOrMethodDecls = append (ast.SubIfcOrMethodDecls, & x)
			skipPunctuationMust (lexer, ";")
		} else {
			// Parse a method decl if starts with "method"
			var x AstIfcDeclMethodDecl
			skipKeywordMust (lexer, "method")
			x.ReturnType = ParseTypeExpr (lexer)
			x.MethodName = ParseIde (lexer)
			rparenFound := skipPunctuationOpt (lexer, "(")
			if rparenFound {
				for {
					isCloseParen := skipPunctuationOpt (lexer, ")")
					if isCloseParen { break }

					argType := ParseTypeExpr (lexer)
					argName := ParseIde (lexer)
					x.ArgTypes = append (x.ArgTypes, argType)
					x.ArgNames = append (x.ArgNames, argName)
					if ! TokenIsSpecificPunctuation (lexer, ")") {
						skipPunctuationMust (lexer, ",")
					}
				}
			}
			ast.SubIfcOrMethodDecls = append (ast.SubIfcOrMethodDecls, & x)
			skipPunctuationMust (lexer, ";")
		}
	}
	return & ast
}

// ================================================================
// Parse 'instance ... endinstance'

// ParseInstance parses BSV declaration: 'instance ... endinstance'
// Current token is 'instance'
func ParseInstance (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseInstance ... ", nil, "\n")

	var astInstance AstInstance

	// Skip past 'instance' keyword
	GetToken (lexer)

	astInstance.TypeclassIde = ParseConstIde (lexer)
	skipPunctuationMust (lexer, "#")
	skipPunctuationMust (lexer, "(")
	for {
		if TokenIsSpecificPunctuation (lexer, ")") { break }
		astInstance.Types = append (astInstance.Types, ParseTypeExpr (lexer))
		if ! TokenIsSpecificPunctuation (lexer, ")") {
			skipPunctuationMust (lexer, ",")
		}
	}
	GetToken (lexer)

	if TokenIsKeyword (lexer, "provisos") {
		astInstance.Provisos = ParseProvisos (lexer)
	}

	skipPunctuationMust (lexer, ";")

	for {
		if TokenIsKeyword (lexer, "endinstance") { break }
		stmt := ParseStmt (lexer)
		astInstance.Stmts = append (astInstance.Stmts, stmt)
	}
	GetToken (lexer)    // 'endinstance'

	if skipPunctuationOpt (lexer, ":") {
		x := ParseConstIde (lexer)
		AST_pp (os.Stdout, "", x)
		// TODO: should be same as astInstance.TypeclassIde
	}

	return & astInstance
}

// ================================================================
// Parse command line

var pp bool = false

func printUsage () () {
	fmt.Fprintf (os.Stdout, "Usage:\n")
	fmt.Fprintf (os.Stdout, "    %s  <optional macro-defs and flags>  filename\n", os.Args [0])
	fmt.Fprintf (os.Stdout, "\n")
	fmt.Fprintf (os.Stdout, "Macro-def: -Dmacro    For `ifdefs in the source file\n")
	fmt.Fprintf (os.Stdout, "Flag:      -debug     Print a verbose trace of the recursive descent parse\n")
	fmt.Fprintf (os.Stdout, "Flag:      -pp        Pretty-print the final AST for the file\n")
}

// ParseCommandLine parses a command line: sequence of -DFOO defs and a filename.
// Returns the -D defs in first result,
// and the filename in the second result.
func ParseCommandLine () ([]string, string) {
	macros        := make ([]string, 0, 0)
	inputFilename := "-"
	for j := 1; j < len (os.Args); j++ {
		arg := os.Args [j]
		if (arg == "-h") || (arg == "--help") {
			printUsage ()
			os.Exit (0)
		} else if strings.HasPrefix (arg, "-D") {
			macros = append (macros, strings.TrimPrefix (arg, "-D"))
		} else if arg == "-debug" {
			debug = true

		} else if arg == "-pp" {
			pp = true

		} else if inputFilename == "-" {
			inputFilename = arg
		} else {
			fmt.Fprintf (os.Stderr, "ERROR: ParseCommandLine: Junk in command line: %s\n", arg)
			os.Exit (1)
		}
	}

	// Debugging
	if false {
		for j, macro := range (macros) {
			if (j == 0) { fmt.Fprintf (os.Stdout, "Macros\n") }
			fmt.Fprintf (os.Stdout, "    %s\n", macro)
		}
		fmt.Fprintf (os.Stdout, "Input filename: %q\n", inputFilename)
	}

	return macros, inputFilename
}

// ================================================================

// TestParser parses a file and prints the ASTs to stdout.
func TestParser () () {
	macros, inputFilename := ParseCommandLine ()

	// Create a lexer for the file
	lexer := NewLexer (inputFilename, macros, nil)
	GetToken (lexer)

	// Parse the file
	ast := ParsePackage (lexer)

	// Pretty-print the AST
	if pp {
		AST_pp (os.Stdout, "", ast)
	} else {
		fmt.Fprintf (os.Stdout, "Ok\n")
	}
}

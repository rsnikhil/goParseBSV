// Part of goParseBSV, a parser for BSV files
// This part defines parsers for the StmtFSM sub-language

package	goParseBSV

import (
	// golang packages
	"os"
)

// ParseFSMstmt parses a FSM statement within a seq, par, if for or while compound
func ParseFSMstmt (lexer *Lexer) (AST) {
	debugPrint (os.Stdout, "ParseFSMstmt ...", nil, "\n")

	var result AST

	if skipKeywordOpt (lexer, "seq") {
		result = ParseFSMseq (lexer)

	} else if skipKeywordOpt (lexer, "par") {
		result = ParseFSMpar (lexer)

	} else if skipKeywordOpt (lexer, "if") {
		result = ParseFSMif (lexer)

	} else if skipKeywordOpt (lexer, "for") {
		result = ParseFSMfor (lexer)

	} else if skipKeywordOpt (lexer, "while") {
		result = ParseFSMwhile (lexer)

	} else if skipKeywordOpt (lexer, "repeat") {
		result = ParseFSMrepeat (lexer)

	} else if skipKeywordOpt (lexer, "return") {
		result = AstFSMreturn {}

	} else if skipKeywordOpt (lexer, "break") {
		result = AstFSMbreak {}

	} else if skipKeywordOpt (lexer, "continue") {
		result = AstFSMcontinue {}

	} else {
		// Parse an Action expression
		result = ParseExpr (lexer)
	}
	skipPunctuationOpt (lexer, ";")

	debugPrint (os.Stdout, "ParseFSMstmt: => ", result, "\n")
	return result
}

// ParseFSMseq parses: seq ... endseq
// Current token is just past the opening 'seq'
func ParseFSMseq (lexer *Lexer) (* AstFSMseq) {
	debugPrint (os.Stdout, "ParseFSMseq ...", nil, "\n")

	var astSeq AstFSMseq

	for {
		if skipKeywordOpt (lexer, "endseq") { break }

		astSeq.Stmts = append (astSeq.Stmts, ParseFSMstmt (lexer))
	}

	debugPrint (os.Stdout, "ParseFSMseq: => ", & astSeq, "\n")
	return & astSeq
}

// ParseFSMpar parses: par ... endpar
// Current token is just past the opening 'par'
func ParseFSMpar (lexer *Lexer) (* AstFSMpar) {
	debugPrint (os.Stdout, "ParseFSMpar ...", nil, "\n")

	var astPar AstFSMpar

	for {
		if skipKeywordOpt (lexer, "endpar") { break }

		astPar.Stmts = append (astPar.Stmts, ParseFSMstmt)
	}

	debugPrint (os.Stdout, "ParseFSMpar: => ", & astPar, "\n")
	return & astPar
}

// ParseFSMIf parses: if (E) S1 [ else S2 ]
// Current token is just past 'if'
func ParseFSMif (lexer *Lexer) (*AstFSMif) {
	debugPrint (os.Stdout, "ParseFSMif ...", nil, "\n")

	var astFSMif AstFSMif

	// Parse the condition
	skipPunctuationMust (lexer, "(")
	astFSMif.ExprCond = ParseExpr (lexer)
	skipPunctuationMust (lexer, ")")

	// Parse the 'then' part
	astFSMif.StmtThen = ParseFSMstmt (lexer)

	// Parse the optional 'else' part
	astFSMif.StmtElse = nil
	if skipKeywordOpt (lexer, "else") {
		astFSMif.StmtElse = ParseFSMstmt (lexer)
	}

	result := & astFSMif
	debugPrint (os.Stdout, "ParseFSMif: => ", result, "\n")
	return result
}

// ParseFSMfor parses: "for (init; cond; incr) body"
// Current token is just past 'for'
func ParseFSMfor (lexer *Lexer) (* AstFSMfor) {
	debugPrint (os.Stdout, "ParseFSMfor ...", nil, "\n")

	var astFor AstFSMfor

	skipPunctuationMust (lexer, "(")
	astFor.LoopInit = ParseStmt (lexer)
	astFor.LoopCond = ParseExpr (lexer)
	skipPunctuationMust (lexer, ";")
	astFor.LoopIncr = ParseStmt (lexer)
	skipPunctuationMust (lexer, ")")
	astFor.LoopBody = ParseFSMstmt (lexer)

	result := & astFor
	debugPrint (os.Stdout, "ParseFSMfor: => ", result, "\n")
	return result
}

// ParseFSMwhile parses: "while (cond) body"
// Current token is just past 'while'
func ParseFSMwhile (lexer *Lexer) (* AstFSMwhile) {
	debugPrint (os.Stdout, "ParseFSMwhile ...", nil, "\n")

	var astFSMwhile AstFSMwhile

	skipPunctuationMust (lexer, "(")
	astFSMwhile.LoopCond = ParseExpr (lexer)
	skipPunctuationMust (lexer, ")")
	astFSMwhile.LoopBody = ParseFSMstmt (lexer)

	result := & astFSMwhile
	debugPrint (os.Stdout, "ParseFSMwhile: => ", result, "\n")
	return result
}

// ParseFSMrepeat parses: "repeat (cond) body"
// Current token is just past 'repeat'
func ParseFSMrepeat (lexer *Lexer) (* AstFSMrepeat) {
	debugPrint (os.Stdout, "ParseFSMrepeat ...", nil, "\n")

	var astFSMrepeat AstFSMrepeat

	skipPunctuationMust (lexer, "(")
	astFSMrepeat.LoopCount = ParseExpr (lexer)
	skipPunctuationMust (lexer, ")")
	astFSMrepeat.LoopBody = ParseFSMstmt (lexer)

	result := & astFSMrepeat
	debugPrint (os.Stdout, "ParseFSMrepeat: => ", result, "\n")
	return result
}

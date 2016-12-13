// This is back-end processor of ASTs created by goParseBSV, a parser
// for BSV files.

// Specifically, this is a pretty printer that traverses an AST
// and prints it out with reasonable indentation etc.

// More generally, it provides an example of how to structure other
// back-ends that traverse the AST and do some processing on them.
// The key idea is that we define a local interface ast_pp with a
// method pp. Then we define the pp method for each AST type AstFoo.
// To process recursive components of interface type AST

package	goParseBSV

import (
	// golang packages
	"fmt"
	"os"
)

// ================================================================
// Top-level function exported out of this file

func AST_pp (fout *os.File, indent string, ast AST) () {
	conv (ast).pp (fout, indent)
}

// ================================================================
// Each kind of AST has its own struct definition
// They are all 'union'd via the AST interface

// AST is the generic type of Abstract Syntax Trees
// PP_AST2 is the method to pretty print an AST
type ast_pp_ifc interface {
	pp (fout *os.File, indent string)
}

// Convert an AST to an ast_pp_ifc
func conv (ast AST) (ast_pp_ifc) {
	ast_pp := ast.(ast_pp_ifc)
	return ast_pp
}

// ================================================================
// Items that occur in many parts of the grammar

// Ides
func (ast *AstIde) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "%s", ast.LeafValue.StringVal)
}

// Attribute instances
func (ast *AstAttrInstance) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "(* ")
	for j, ide := range (ast.Ides) {
		if (j != 0) { fmt.Fprintf (fout, ", ") }
		ide.pp (fout, indent)
		if ast.Vals [j] != nil {
			fmt.Fprintf (fout, " = ")
			conv (ast.Vals [j]).pp (fout, indent)
		}
	}
	fmt.Fprintf (fout, " *)")
}

// ================================================================
// ASTs for types

// AstTypeNum is a BSV numeric type, e.g.,  "16"
func (ast *AstTypeNum) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "%d", ast.LeafValue.IntVal)
}

// AstTypeVar is a BSV type variable (identifier with lowercase first letter)
// E.g.,  "t"
func (ast *AstTypeVar) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "%s", ast.LeafValue.StringVal)
}

// AstTypeConstructed is a BSV "TypeConstructor #(typeExpr, ..., typeExpr)"
func (x AstTypeConstructed) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "%s", x.Constructor.LeafValue.StringVal)
	if len (x.Args) > 0 {
		fmt.Fprintf (fout, " #(")
		for j, arg := range (x.Args) {
			if j != 0 { fmt.Fprintf (fout, ", ") }
			conv (arg).pp (fout, indent)
		}
		fmt.Fprintf (fout, ")")
	}
}

// AstTypedefDefinedAsStruct is a BSV "struct { type field; ... ; type field }"
func (ast *AstTypedefDefinedAsStruct) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "struct {")
	indent2 := indent + "        "
	for j, memberName := range (ast.StructMemberNames) {
		if (j > 0) { fmt.Fprintf (fout, indent2) }
		fieldType := ast.StructMemberTypes [j]
		conv (fieldType).pp (fout, indent)
		fmt.Fprintf (fout, " ")
		memberName.pp (fout, indent)
		fmt.Fprintf (fout, ";\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "}")
}

// AstTypedefDefinedAsTaggedUnion is a BSV "union tagged { type field; ... ; type field }"
func (ast *AstTypedefDefinedAsTaggedUnion) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "union tagged {\n")
	indent2 := indent + "             "
	for j, memberName := range (ast.TaggedUnionMemberNames) {
		if (j > 0) { fmt.Fprintf (fout, indent2) }
		fieldType := ast.TaggedUnionMemberTypes [j]
		conv (fieldType).pp (fout, indent)
		fmt.Fprintf (fout, " ")
		memberName.pp (fout, indent)
		fmt.Fprintf (fout, ";\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "}")
}

// AstTypedefDefinedAsEnum is a BSV "enum { label [=val], ... , label [=val] }"
func (ast *AstTypedefDefinedAsEnum) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "enum {")
	indent2 := indent + "      "
	for j, typedefEnumElement := range (ast.TypedefEnumElements) {
		if (j > 0) { fmt.Fprintf (fout, indent2) }
		typedefEnumElement.pp (fout, indent)
		if ast.TypedefEnumVals [j] != nil {
			fmt.Fprintf (fout, " = ")
			conv (ast.TypedefEnumVals [j]).pp (fout, indent2)
		}
		if j < (len (ast.TypedefEnumElements) - 1) {
			fmt.Fprintf (fout, ",\n")
		} else {
			fmt.Fprintf (fout, "}")
		}
	}
}

// AstTypedefDefinee is the new type being defined in a typedef: Type #(typeFormal,...,typeFormal)
// where each typeformal is "type typevar" or "numeric type typevar"
func (ast *AstTypedefDefinee) pp (fout *os.File, indent string) {
	conv (ast.TypeConstructor).pp (fout, indent)
	if len (ast.TypeFormals) != 0 {
		fmt.Fprintf (fout, " #(")
		for j, formal := range (ast.TypeFormals) {
			if (j != 0) { fmt.Fprintf (fout, ", ") }
			if ast.TypeFormalKinds [j] == KindNumeric {
				fmt.Fprintf (fout, "numeric ")
			}
			fmt.Fprintf (fout, "type ")
			conv (formal).pp (fout, indent)
		}
		fmt.Fprintf (fout, ")")
	}
}

// ================================================================
// ASTs for exprs

// AstNum is a BSV numeric constant
// E.g., 23
func (ast *AstNum) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "%d", ast.LeafValue.IntVal)
}

// AstString is a BSV string
// E.g.,  "x"
func (ast *AstString) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "\"%s\"", ast.LeafValue.StringVal)
}

// AstExpr is the parse of a BSV expression applying Expr0 to Exprs
func (ast *AstExpr) pp (fout *os.File, indent string) () {
	e0 := ast.Expr0
	es := ast.Exprs

	conv (e0).pp (fout, indent)
	fmt.Fprintf (fout, " (")
	for j,ej := range es {
		if (j > 0) {
			fmt.Fprintf (fout, ", ")
		}
		conv (ej).pp (fout, indent)
	}
	fmt.Fprintf (fout, ")")
}

// AstCondPredicate is the parse of a condition, in if, while, rule conditions, method conditions, etc.
// i.e., conjunct &&& conjunct &&& ...
func (ast *AstCondPredicate) pp (fout *os.File, indent string) () {
	for j, conjunct := range (ast.Conjuncts) {
		conv (conjunct).pp (fout, indent)
		if j < (len (ast.Conjuncts) - 1) {
			fmt.Fprintf (fout, "\n" + indent + "&&& ")
		}
	}
}

// AstCondPattern is the parse of: expr matches pattern
func (ast *AstCondPattern) pp (fout *os.File, indent string) () {
	conv (ast.Expr).pp (fout, indent)
	fmt.Fprintf (fout, " matches ")
	conv (ast.Pattern).pp (fout, indent)
}

// AstBlock is the parse of:
//     action ... endaction
//     actionvalue ... endactionvalue
//     begin ... end
func (ast *AstBlock) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, ast.BlockKind)
	if ast.BlockName != nil {
		fmt.Fprintf (fout, ": ")
		conv (ast.BlockName).pp (fout, indent)
	}
	fmt.Fprintf (fout, "\n")

	indent2 := indent + "   "
	for _, stmt := range (ast.Stmts) {
		fmt.Fprintf (fout, indent2)
		conv (stmt).pp (fout, indent2)
		fmt.Fprintf (fout, "\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "%s", MatchingEndKeyword (ast.BlockKind))
}

// AstReturn is the parse of: return Expr
func (ast *AstReturn) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "return ")
	conv (ast.Expr).pp (fout, indent)
	fmt.Fprintf (fout, ";")
}

// AstStructExpr is the parse of: Ide {member:Expr, ... }
func (ast *AstStructExpr) pp (fout *os.File, indent string) () {
	conv (ast.Name).pp (fout, indent)
	fmt.Fprintf (fout, " {\n")
	indent2 := indent + "   "
	for j, name := range (ast.MemberNames) {
		fmt.Fprintf (fout, indent2)
		conv (name).pp (fout, indent2)
		fmt.Fprintf (fout, ":")
		conv (ast.MemberExprs [j]).pp (fout, indent2)
		if j < (len (ast.MemberNames) - 1) {
			fmt.Fprintf (fout, ",\n")
		} else {
			fmt.Fprintf (fout, "}")
		}
	}
}

// AstTaggedUnionExpr is the parse of: tagged Ide Expr
func (ast *AstTaggedUnionExpr) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "tagged ")
	indent2 := indent + "       "
	conv (ast.Name).pp (fout, indent)
	if ast.Expr != nil {
		fmt.Fprintf (fout, "\n" + indent2)
		conv (ast.Expr).pp (fout, indent2)
	}
}

// AstCase is the parse of: case ... endcase
func (ast *AstCase) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "case (")
	conv (ast.Expr).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	for j, label := range (ast.Labels) {
		fmt.Fprintf (fout, indent2)
		conv (label).pp (fout, indent2)
		fmt.Fprintf (fout, ":")
		conv (ast.Exprs [j]).pp (fout, indent2 + "   ")
		fmt.Fprintf (fout, ";\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endcase")
}

// AstPatternCase is the parse of: case () matches ... endcase
func (ast *AstPatternCase) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "case (")
	conv (ast.Expr).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	for j, pattern := range (ast.Patterns) {
		fmt.Fprintf (fout, indent2)
		conv (pattern).pp (fout, indent2)
		fmt.Fprintf (fout, ":")
		conv (ast.Exprs [j]).pp (fout, indent2 + "   ")
		fmt.Fprintf (fout, ";\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endcase")
}

// ===============================================================
// ASTs for statements

// VarDecl is the parse of 'type x1 = e1, x2 = e2, ...;"
func (ast *AstVarDecl) pp (fout *os.File, indent string) () {
	conv (ast.Type).pp (fout, indent)
	fmt.Fprintf (fout, "\n")
	indent2 := indent + "   "
	for j, varinit := range (ast.VarInits) {
		fmt.Fprintf (fout, indent2)
		varinit.pp (fout, indent)
		if j < (len (ast.VarInits) - 1) {
			fmt.Fprintf (fout, ",\n")
		} else {
			fmt.Fprintf (fout, ";")
		}
	}
}

// An Init in a VarDecl, 'xJ = eJ' or 'Ide [e]...[e] = Expr;'
func (ast *AstVarInit) pp (fout *os.File, indent string) () {
	conv (ast.Ide).pp (fout, indent)
	indent2 := indent + "   "
	for _, arrayDim := range (ast.ArrayDims) {
		fmt.Fprintf (fout, " [")
		conv (arrayDim).pp (fout, indent2)
		fmt.Fprintf (fout, " ]")
	}
	if ast.Kind != BindingKindNone {
		if ast.Kind == BindingKindEq {
			fmt.Fprintf (fout, " = ")
		} else {
			fmt.Fprintf (fout, " <- ")
		}
		conv (ast.Init).pp (fout, indent2)
	}
}

// AstLet is the parse of a BSV statement: "let x = e" or "let x <- e"
func (ast *AstLet) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "let ")
	conv (ast.Ide).pp (fout, indent)
	if ast.Kind != BindingKindNone {
		indent2 := indent + "   "
		if ast.Kind == BindingKindEq {
			fmt.Fprintf (fout, " = ")
		} else {
			fmt.Fprintf (fout, " <- ")
		}
		conv (ast.Expr).pp (fout, indent2)
		fmt.Fprintf (fout, ";")
	}
}

// AstMatch is the parse of a BSV statement: "match pattern = e"  or "match pattern <- e"
func (ast *AstMatch) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "match ")
	conv (ast.Pattern).pp (fout, indent)
	fmt.Fprintf (fout, " = ")
	conv (ast.Expr).pp (fout, indent + "   ")
}

// AstAssign is the parse of a BSV statement: "lhs = e" or "ide <- e"
func (ast *AstAssign) pp (fout *os.File, indent string) () {
	conv (ast.Lhs).pp (fout, indent)
	if ast.Kind != BindingKindNone {
		if ast.Kind == BindingKindEq {
			fmt.Fprintf (fout, " = ")
		} else {
			fmt.Fprintf (fout, " <- ")
		}
		conv (ast.Rhs).pp (fout, indent + "   ")
		fmt.Fprintf (fout, ";")
	}
}

// AstRule is the parse of "rule ... endrule"
func (ast *AstRule) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "rule ")
	conv (ast.Name).pp (fout, indent)
	fmt.Fprintf (fout, " (")
	if ast.Cond != nil {
		conv (ast.Cond).pp (fout, indent)
	}
	fmt.Fprintf (fout, ");\n")
	indent2 := indent + "   "
	for _, stmt := range (ast.Stmts) {
		fmt.Fprintf (fout, indent2)
		conv (stmt).pp (fout, indent2)
		fmt.Fprintf (fout, "\n")
	}
	fmt.Fprintf (fout, indent + "endrule")
}

// AstFunctionProto is the parse of "function type name (type formal, ..., type formal)"
func (ast *AstFunctionProto) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "function ")
	conv (ast.ResultType).pp (fout, indent)
	fmt.Fprintf (fout, " ")
	conv (ast.Name).pp (fout, indent)
	fmt.Fprintf (fout, " (")
	for j, formal := range (ast.Formals) {
		if (j != 0) { fmt.Fprintf (fout, ", ") }
		conv (ast.FormalTypes [j]).pp (fout, indent)
		fmt.Fprintf (fout, " ")
		conv (formal).pp (fout, indent)
	}
	fmt.Fprintf (fout, ")")
}

// AstFunctionDef is the parse of "function ... endfunction" or "function .... = e"
func (ast *AstFunctionDef) pp (fout *os.File, indent string) () {
	ast.Proto.pp (fout, indent)

	if len (ast.Provisos) == 0 {
		fmt.Fprintf (fout, ";\n")
	} else {
		fmt.Fprintf (fout, "\n    provisos (")
		for j, proviso := range (ast.Provisos) {
			if j != 0 { fmt.Fprintf (fout, "\n           , ") }
			conv (proviso).pp (fout, indent)
		}
		fmt.Fprintf (fout, ");\n")
	}

	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.Body).pp (fout, indent2)
	fmt.Fprintf (fout, "\n")

	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endfunction")
}

// AstMethodDef is the parse of "method ... endmethod" or "method .... = e"
func (ast *AstMethodDef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "method ")
	conv (ast.ResultType).pp (fout, indent)
	fmt.Fprintf (fout, " ")
	conv (ast.Name).pp (fout, indent)
	fmt.Fprintf (fout, " (")
	for j, formal := range (ast.Formals) {
		if (j != 0) { fmt.Fprintf (fout, ", ") }
		conv (ast.FormalTypes [j]).pp (fout, indent)
		fmt.Fprintf (fout, " ")
		conv (formal).pp (fout, indent)
	}
	if ast.Cond != nil {
		fmt.Fprintf (fout, " if (")
		conv (ast.Cond).pp (fout, indent)
		fmt.Fprintf (fout, ")")
	}
	fmt.Fprintf (fout, ");\n")

	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.Body).pp (fout, indent2)
	fmt.Fprintf (fout, "\n")

	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endmethod")
}

// AstModuleDef is the parse of "module ... endmodule"
func (ast *AstModuleDef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "module")
	if ast.ModuleType != nil {
		fmt.Fprintf (fout, " [")
		conv (ast.ModuleType).pp (fout, indent)
		fmt.Fprintf (fout, "]")
	}

	fmt.Fprintf (fout, " ")
	conv (ast.Name).pp (fout, indent)

	fmt.Fprintf (fout, " #(")
	for j, param := range (ast.FormalParams) {
		if (j != 0) { fmt.Fprintf (fout, ", ") }
		conv (ast.FormalParamTypes [j]).pp (fout, indent)
		fmt.Fprintf (fout, " ")
		conv (param).pp (fout, indent)
	}
	fmt.Fprintf (fout, ")")

	fmt.Fprintf (fout, " (")
	conv (ast.IfcType).pp (fout, indent)
	fmt.Fprintf (fout, ")")


	if len (ast.Provisos) == 0 {
		fmt.Fprintf (fout, ";\n")
	} else {
		fmt.Fprintf (fout, "\n    provisos (")
		for j, proviso := range (ast.Provisos) {
			if j != 0 { fmt.Fprintf (fout, "\n           , ") }
			conv (proviso).pp (fout, indent)
		}
		fmt.Fprintf (fout, ");\n")
	}

	indent2 := indent + "   "
	for _, stmt := range (ast.Stmts) {
		fmt.Fprintf (fout, indent2)
		conv (stmt).pp (fout, indent2)
		fmt.Fprintf (fout, "\n\n")
	}

	fmt.Fprintf (fout, indent + "endmodule")
}

// AstInterfaceDef is the parse of "interface ... endinterface" within a module
func (ast *AstInterfaceDef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "interface ")
	if ast.Type != nil {
		conv (ast.Type).pp (fout, indent)
		fmt.Fprintf (fout, " ")
	}
	conv (ast.Name).pp (fout, indent)
	fmt.Fprintf (fout, ";\n")

	indent2 := indent + "   "
	for _, def := range (ast.MethodAndIfcDefs) {
		fmt.Fprintf (fout, indent2)
		conv (def).pp (fout, indent2)
		fmt.Fprintf (fout, "\n")
	}

	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endinterface")
}

// AstInterfaceAssign is the parse of "interface ... = e" within a module
func (ast *AstInterfaceAssign) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "interface ")
	if ast.Type != nil {
		conv (ast.Type).pp (fout, indent)
		fmt.Fprintf (fout, " ")
	}
	conv (ast.Name).pp (fout, indent)
	fmt.Fprintf (fout, " = ")
	conv (ast.Val).pp (fout, indent + "   ")
}

// AstIf is the parse of: "if (E) S1 else S2"
func (ast *AstIf) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "if (")
	conv (ast.ExprCond).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.StmtThen).pp (fout, indent2)
	if ast.StmtElse != nil {
		fmt.Fprintf (fout, "\n" + indent + "else\n" + indent2)
		conv (ast.StmtElse).pp (fout, indent2)
	}
}

// AstFor is the parse of: "for (type x = e; e; x = ...) stmt"
func (ast *AstFor) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "for (")
	conv (ast.LoopInit).pp (fout, indent)
	fmt.Fprintf (fout, ";")
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, ";")
	conv (ast.LoopIncr).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.LoopBody).pp (fout, indent2)
}

// AstWhile is the parse of: "while (e) stmt"
func (ast *AstWhile) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "while (")
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.LoopBody).pp (fout, indent2)
}

// AstInterfaceExpr is the parse of expression: interface ... endinterface
func (ast *AstInterfaceExpr) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "interface ")
	if ast.Type != nil {
		conv (ast.Type).pp (fout, indent)
	}
	fmt.Fprintf (fout, ";\n")

	indent2 := indent + "   "
	for _, def := range (ast.MethodAndIfcDefs) {
		fmt.Fprintf (fout, indent2)
		conv (def).pp (fout, indent2)
		fmt.Fprintf (fout, "\n")
	}

	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endinterface")
}

// ================================================================
// StmtFSM

// AstFSMseq is the parse of: 'seq stmt stmt ... endseq'
func (ast *AstFSMseq) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "seq\n")
	indent2 := indent + "   "
	for _, stmt := range (ast.Stmts) {
		fmt.Fprintf (fout, indent2)
		conv (stmt).pp (fout, indent2)
		fmt.Fprintf (fout, "\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endseq")
}

// AstFSMpar is the parse of: 'par stmt stmt ... endpar'
func (ast *AstFSMpar) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "par\n")
	indent2 := indent + "   "
	for _, stmt := range (ast.Stmts) {
		fmt.Fprintf (fout, indent2)
		conv (stmt).pp (fout, indent2)
		fmt.Fprintf (fout, "\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endpar")
}

// AstFSMif is the parse of: "if (E) S1 else S2"
func (ast *AstFSMif) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "if (")
	conv (ast.ExprCond).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.StmtThen).pp (fout, indent2)
	if ast.StmtElse != nil {
		fmt.Fprintf (fout, "\n" + indent + "else\n" + indent2)
		conv (ast.StmtElse).pp (fout, indent2)
	}
}

// AstFSMfor is the parse of: 'for (init_stmts; cond; incr_stmts) fsmstmt'
func (ast *AstFSMfor) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "for (")
	conv (ast.LoopInit).pp (fout, indent)
	fmt.Fprintf (fout, ";")
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, ";")
	conv (ast.LoopIncr).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.LoopBody).pp (fout, indent2)
}

// AstFSMwhile is the parse of: "while (e) stmt"
func (ast *AstFSMwhile) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "while (")
	conv (ast.LoopCond).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.LoopBody).pp (fout, indent2)
}

// AstFSMrepeat is the parse of: "repeat (n) stmt"
func (ast *AstFSMrepeat) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "repeat (")
	conv (ast.LoopCount).pp (fout, indent)
	fmt.Fprintf (fout, ")\n")
	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	conv (ast.LoopBody).pp (fout, indent2)
}

// AstFSMreturn is the parse of: 'return'
func (ast *AstFSMreturn) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "return")
}

// AstFSMbreak is the parse of: 'break'
func (ast *AstFSMbreak) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "break")
}

// AstFSMcontinue is the parse of: 'continue'
func (ast *AstFSMcontinue) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "continue")
}

// ================================================================
// ASTs for patterns
// Syntax of patterns:
//     Pattern variables
//         . *                                        wildcard
//         . x                                        pattern varIde
//     Pattern constants
//         23
//         2.5
//         "Hello"
//         Foo                                        enum labels
//     Tagged unions
//         tagged tag pattern
//     Structs
//         tagged structname {member:pattern, ...}
//     Tuples
//         { pattern, ... }

// AstPatternVarIde is the parse of a pattern .* or .x
func (ast *AstPatternVarIde) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, ".")
	conv (ast.varIde).pp (fout, indent)
}

// AstPatternConst is the parse of a pattern const integer, real, string, or Enum label
func (ast *AstPatternConst) pp (fout *os.File, indent string) () {
	conv (ast.constant).pp (fout, indent)
}

// AstStructPattern is the parse of:  tagged StructName { MemberName: Pattern, ..., }
// and Tuple patterns (StructName is "Tuple", MemberNames are tuple_1, tuple_2, ...
func (ast *AstStructPattern) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "struct ")
	conv (ast.StructName).pp (fout, indent)
	fmt.Fprintf (fout, " {")
	indent2 := indent + "   "
	for j, memberName := range ast.MemberNames {
		conv (memberName).pp (fout, indent2)
		fmt.Fprintf (fout, ":")
		conv (ast.MemberPatterns [j]).pp (fout, indent2)
		if j < (len (ast.MemberNames) - 1) {
			fmt.Fprintf (fout, ",")
		} else {
			fmt.Fprintf (fout, "}")
		}
	}
}

// AstTaggedUnionPattern is the parse of:  tagged UnionName [ Pattern ]
func (ast *AstTaggedUnionPattern) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "tagged union ")
	indent2 := indent + "   "
	conv (ast.TaggedUnionName).pp (fout, indent2)
	if ast.MemberPattern != nil {
		fmt.Fprintf (fout, " (")
		conv (ast.MemberPattern).pp (fout, indent2)
		fmt.Fprintf (fout, ")")
	}
}

// ================================================================
// Top-level constructs in a package

// AstPackage is parse of 'package pkgStmt ... pkgStmt endpackage'
func (ast *AstPackage) pp (fout *os.File, indent string) {
	if ast.PackageName != nil {
		fmt.Fprintf (fout, "package ")
		conv (ast.PackageName).pp (fout, indent)
		fmt.Fprintf (fout, ";\n")
	}

	indent2 := indent + "   "
	for _, decl := range ast.PackageStmts {
		fmt.Fprintf (fout, indent2)
		conv (decl).pp (fout, indent2)
		fmt.Fprintf (fout, "\n\n")
	}

	if ast.PackageName != nil {
		fmt.Fprintf (fout, indent)
		fmt.Fprintf (fout, "endpackage: ")
		conv (ast.PackageName).pp (fout, indent)
		fmt.Fprintf (fout, "\n")
	}
}

// AstImport is parse of 'import x :: *;'
func (ast *AstImport) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "import ")
	conv (ast.PackageName).pp (fout, indent)
	fmt.Fprintf (fout, "    :: *;")
}

// AstExport is parse of 'export x, y, z (..), w, ...;'
func (ast *AstExport) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "export ")
	indent2 := indent + "       "
	for j, ide := range (ast.Ides) {
		conv (ide).pp (fout, indent2)
		if ast.WithMembers [j] { fmt.Fprintf (fout, " (..)") }
		if j == (len (ast.Ides) - 1) {
			fmt.Fprintf (fout, ";")
		} else {
			fmt.Fprintf (fout, ",\n")
			fmt.Fprintf (fout, indent2)
		}
	}
}

// AstImportBDPI is parse of 'import "BDPI" function_proto'
func (ast *AstImportBDPI) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "import \"BDPI\"\n")

	// TODO: ImportBDPI does not yet handle renaming of the imported function

	indent2 := indent + "   "
	fmt.Fprintf (fout, indent2)
	ast.Proto.pp (fout, indent2)
	fmt.Fprintf (fout, ";")
}

// AstTypedef is the parse of a BSV statement: "typedef typeDefinedAs newtype deriving (typeclass, ...);"
func (ast *AstTypedef) pp (fout *os.File, indent string) () {
	fmt.Fprintf (fout, "typedef ")
	indent2 := indent + "        "
	conv (ast.TypedefDefinedAs).pp (fout, indent2)
	fmt.Fprintf (fout, " ")
	conv (ast.TypedefDefinee).pp (fout, indent2)
	if len (ast.TypeclassIdes) > 0 {
		fmt.Fprintf (fout, "\n" + indent + "deriving ")
		for j, deriveType := range (ast.TypeclassIdes) {
			if j == 0 {
				fmt.Fprintf (fout, "(")
			} else {
				fmt.Fprintf (fout, ", ")
			}
			conv (deriveType).pp (fout, indent2)
		}
		fmt.Fprintf (fout, ")");
	}
	fmt.Fprintf (fout, ";")
}

// AstIfcDecl is the parse of a top-level BSV declaration: 'interface ... endinterface'
func (ast *AstIfcDecl) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "interface ")
	conv (ast.Ifc).pp (fout, indent)
	fmt.Fprintf (fout, ";\n")
	indent2 := indent + "   "
	for _, subIfcOrMethodDecl := range (ast.SubIfcOrMethodDecls) {
		fmt.Fprintf (fout, indent2)
		conv (subIfcOrMethodDecl).pp (fout, indent2)
		fmt.Fprintf (fout, ";\n")
	}
	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endinterface")
}

// Sub-interface within an interface declaration
func (ast *AstIfcDeclSubIfcDecl) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "interface ")
	conv (ast.SubIfcType).pp (fout, indent)
	fmt.Fprintf (fout, " ")
	conv (ast.SubIfcName).pp (fout, indent)
}

// Method declaration within an interface declaration
func (ast *AstIfcDeclMethodDecl) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "method ")
	conv (ast.ReturnType).pp (fout, indent)
	fmt.Fprintf (fout, " ")
	conv (ast.MethodName).pp (fout, indent)
	if len (ast.ArgNames) != 0 {
		fmt.Fprintf (fout, " (")
		for j, argName := range (ast.ArgNames) {
			argType := ast.ArgTypes [j]
			conv (argType).pp (fout, indent)
			fmt.Fprintf (fout, " ")
			conv (argName).pp (fout, indent)
			if (j < len (ast.ArgNames) - 1) {
				fmt.Fprintf (fout, ",")
			}
		}
		fmt.Fprintf (fout, ")")
	}
}

//     instance typeclassIde # ( type { , type } ) [ provisos ] ;
//         { varAssign ; | functionDef | moduleDef }
//     endinstance [ : typeclassIde ]
func (ast *AstInstance) pp (fout *os.File, indent string) {
	fmt.Fprintf (fout, "instance ")
	conv (ast.TypeclassIde).pp (fout, indent)
	fmt.Fprintf (fout, " #(")
	for j, ty := range (ast.Types) {
		if j != 0 { fmt.Fprintf (fout, ", ") }
		conv (ty).pp (fout, indent)
	}
	fmt.Fprintf (fout, ")")

	if len (ast.Provisos) == 0 {
		fmt.Fprintf (fout, ";\n")
	} else {
		fmt.Fprintf (fout, "\n")
		fmt.Fprintf (fout, indent + "   provisos (")
		indent2 :=         indent + "             "
		for j, proviso := range (ast.Provisos) {
			conv (proviso).pp (fout, indent2)
			if j == (len (ast.Provisos) - 1) {
				fmt.Fprintf (fout, ");\n")
			} else {
				fmt.Fprintf (fout, ",\n")
				fmt.Fprintf (fout, indent2)
			}
		}
	}

	indent3 := indent + "   "
	for _, stmt := range ast.Stmts {
		fmt.Fprintf (fout, indent3)
		conv (stmt).pp (fout, indent3)
		fmt.Fprintf (fout, "\n")
	}

	fmt.Fprintf (fout, indent)
	fmt.Fprintf (fout, "endinstance")
}

// ================================================================

// Part of goParseBSV, a parser for BSV files
// This part defines ASTs for BSV

package	goParseBSV

// ================================================================
// Each kind of AST has its own struct definition
// They are all 'union'd via the AST interface
// (Go does not seem to have an actual 'union' or 'tagged union' type)

// AST is the generic type of Abstract Syntax Trees
type AST interface {
}

// Terminolgy:
//   Definee:   the thing being defined                 (classically: Definiendum, cf. Wikipedia)
//   DefinedAs: the expression that defines the definee (classically: Definiens,   cf. Wikipedia)

// ================================================================
// Items that occur in many parts of the grammar

// Ides
type AstIde struct {
	LeafValue  *Token    // TokIde
}

// Attribute instances
type AstAttrInstance struct {
	Ides         [] *AstIde
	Vals         [] AST
}

// ================================================================
// ASTs for types

// AstTypeNum is a BSV numeric type, e.g.,  "16"
type AstTypeNum struct {
	LeafValue  *Token    // TokInteger
}

// AstTypeVar is a BSV type variable (identifier with lowercase first letter)
// E.g.,  "t"
type AstTypeVar struct {
	LeafValue  *Token    // TokIde
}

// AstTypeConstructed is a BSV "TypeConstructor #(typeExpr, ..., typeExpr)"
type AstTypeConstructed struct {
	Constructor  *AstIde
	Args         [] AST
}

// AstTypedefDefinedAsStruct is a BSV "struct { type field; ... ; type field }"
type AstTypedefDefinedAsStruct struct {
	StructMemberNames  [] *AstIde
	StructMemberTypes  [] AST
}

// AstTypedefDefinedAsTaggedUnion is a BSV "union tagged { type field; ... ; type field }"
type AstTypedefDefinedAsTaggedUnion struct {
	TaggedUnionMemberNames  [] *AstIde
	TaggedUnionMemberTypes  [] AST
}

// AstTypedefDefinedAsEnum is a BSV "enum { label [=val], ... , label [=val] }"
type AstTypedefDefinedAsEnum struct {
	TypedefEnumElements  [] *AstIde
	TypedefEnumVals      [] *AstNum
}

// Type kinds: either a 'value' type or a 'numeric' type
const (
	KindValue    uint = iota
	KindNumeric
)

// AstTypedefDefinee is the new type being defined in a typedef: Type #(typeFormal,...,typeFormal)
// where each typeformal is "type typevar" or "numeric type typevar"
type AstTypedefDefinee struct {
	TypeConstructor  *AstIde
	TypeFormals       [] *AstIde
	TypeFormalKinds   [] uint
}

// ================================================================
// ASTs for exprs
//     Integer constant
//     Ide
//     Expr.identifier                // struct field selection, interface method selection
//     op Expr                        // unary prefix operator
//     Expr op Expr                   // binary infix operator
//     Expr (Expr, ..., Expr)         // function application
//     Expr [Expr]                    // array selection
//     (Expr)
//     action ... endaction
//     begin ... end
//     actionvalue ... endactionvalue
//     return Expr
//     Ide { member:expr, ... }       // struct
//     interface ... endinterface

// AstNum is a BSV numeric constant
// E.g., 23
type AstNum struct {
	LeafValue  *Token    // TokInteger
}

// AstString is a BSV string
// E.g.,  "x"
type AstString struct {
	LeafValue  *Token    // TokString
}

// AstExpr is the parse of a BSV expression applying Expr0 to Exprs
type AstExpr struct {
	Expr0   AST
	Exprs   [] AST
}

// AstCondPredicate is the parse of a condition, in if, while, rule conditions, method conditions, etc.
// i.e., conjunct &&& conjunct &&& ...
type AstCondPredicate struct {
	Conjuncts [] AST
}

// AstCondPattern is the parse of: expr matches pattern
type AstCondPattern struct {
	Expr     AST
	Pattern  AST
}

// AstBlock is the parse of:
//     action ... endaction
//     actionvalue ... endactionvalue
//     begin ... end
type AstBlock struct {
	BlockKind  string    // "action", "actionvalue"  or "begin"
	BlockName  *AstIde
	Stmts []   AST
}

// AstReturn is the parse of: return Expr
type AstReturn struct {
	Expr  AST
}

// AstStructExpr is the parse of: Ide {member:Expr, ... }
type AstStructExpr struct {
	Name         *AstIde
	MemberNames  [] (*AstIde)
	MemberExprs  [] AST
}

// AstTaggedUnionExpr is the parse of: tagged Ide Expr
type AstTaggedUnionExpr struct {
	Name         *AstIde
	Expr         AST
}

// AstCase is the parse of: case ... endcase
type AstCase struct {
	Expr         AST
	Labels       [] AST
	Exprs        [] AST
}

// AstPatternCase is the parse of: case () matches ... endcase
type AstPatternCase struct {
	Expr         AST
	Patterns     [] AST
	Exprs        [] AST
}

// AstInterfaceExpr is the parse of expression: interface ... endinterface
type AstInterfaceExpr struct {
	Type              *AstIde
        MethodAndIfcDefs  [] AST
}

// ===============================================================
// ASTs for statements

// VarDecl is the parse of 'type x1 = e1, x2 = e2, ...;"
type AstVarDecl struct {
	Type        AST
	VarInits [] *AstVarInit
}

// Kinds of var declarations and assignments
// e.g., "TypeExpr  identifier;"
// e.g., "TypeExpr  identifier  = Expr;"
// e.g., "TypeExpr  identifier <- Expr;"
const (
	BindingKindNone    uint = iota
	BindingKindEq                    // =
	BindingKindLArrow                // <-
)

// An Init in a VarDecl, 'xJ = eJ' or 'Ide [e]...[e] = Expr;'
type AstVarInit struct {
	Ide        *AstIde
	ArrayDims  [] AST
	Kind       uint
	Init       AST
}

// AstLet is the parse of a BSV statement: "let x = e" or "let x <- e"
type AstLet struct {
	Ide     *AstIde
	Kind    uint
	Expr    AST
}

// AstMatch is the parse of a BSV statement: "match pattern = e"  or "match pattern <- e"
type AstMatch struct {
	Pattern    AST
	Expr       AST
}

// AstAssign is the parse of a BSV statement: "lhs = e" or "ide <- e"
type AstAssign struct {
	Lhs     AST
	Kind    uint
	Rhs     AST
}

// AstRule is the parse of "rule ... endrule"
type AstRule struct {
	Name         *AstIde
	Cond         AST
	Stmts        [] AST
}

// AstFunctionProto is the parse of "function type name (type formal, ..., type formal)"
type AstFunctionProto struct {
	ResultType   AST
	Name         *AstIde
	Formals      [] AST
	FormalTypes  [] AST
}

// AstFunctionDef is the parse of "function ... endfunction" or "function .... = e"
type AstFunctionDef struct {
	Proto        *AstFunctionProto
	Provisos     [] AST
	Body         AST
}

// AstMethodDef is the parse of "method ... endmethod" or "method .... = e"
type AstMethodDef struct {
	ResultType   AST
	Name         *AstIde
	Formals      [] AST
	FormalTypes  [] AST
	Cond         AST
	Body         AST
}

// AstModuleDef is the parse of "module ... endmodule"
type AstModuleDef struct {
	ModuleType        AST
	Name              *AstIde
	FormalParams      [] AST
	FormalParamTypes  [] AST
	IfcType           AST        // TODO: Grammar says this can be a list of ide,type?
	Provisos          [] AST
	Stmts             [] AST
}

// AstInterfaceDef is the parse of "interface ... endinterface" within a module
type AstInterfaceDef struct {
	Type              AST
	Name              *AstIde
        MethodAndIfcDefs  [] AST
}

// AstInterfaceAssign is the parse of "interface ... = e" within a module
type AstInterfaceAssign struct {
	Type  AST
	Name  *AstIde
	Val   AST
}

// AstIf is the parse of: "if (E) S1 else S2"
type AstIf struct {
	ExprCond     AST
	StmtThen     AST
	StmtElse     AST
}

// AstFor is the parse of: "for (type x = e; e; x = ...) stmt"
type AstFor struct {
	LoopInit     AST
	LoopCond     AST
	LoopIncr     AST
	LoopBody     AST
}

// AstWhile is the parse of: "while (e) stmt"
type AstWhile struct {
	LoopCond     AST
	LoopBody     AST
}

// ================================================================
// StmtFSM

// AstFSMseq is the parse of: 'seq stmt stmt ... endseq'
type AstFSMseq struct {
	Stmts  [] AST
}

// AstFSMpar is the parse of: 'par stmt stmt ... endpar'
type AstFSMpar struct {
	Stmts  [] AST
}

// AstFSMif is the parse of: 'if (cond) fsmstmt [else fsmstmt]'
type AstFSMif struct {
	ExprCond  AST
	StmtThen  AST
	StmtElse  AST
}

// AstFSMfor is the parse of: 'for (init_stmts; cond; incr_stmts) fsmstmt'
type AstFSMfor struct {
	LoopInit     AST
	LoopCond     AST
	LoopIncr     AST
	LoopBody     AST
}

// AstFSMwhile is the parse of: 'while (cond) fsmstmt'
type AstFSMwhile struct {
	LoopCond  AST
	LoopBody  AST
}

// AstFSMrepeat is the parse of: 'repeat (n) fsmstmt'
type AstFSMrepeat struct {
	LoopCount  AST
	LoopBody   AST
}

// AstFSMreturn is the parse of: 'return'
type AstFSMreturn struct {
}

// AstFSMbreak is the parse of: 'break'
type AstFSMbreak struct {
}

// AstFSMcontinue is the parse of: 'continue'
type AstFSMcontinue struct {
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
type AstPatternVarIde struct {
	varIde AST
}

// AstPatternConst is the parse of a pattern const integer, real, string, or Enum label
type AstPatternConst struct {
	constant AST
}

// AstStructPattern is the parse of:  tagged StructName { MemberName: Pattern, ..., }
// and Tuple patterns (StructName is "Tuple", MemberNames are tuple_1, tuple_2, ...
type AstStructPattern struct {
	StructName     AST
	MemberNames    [] AST
	MemberPatterns [] AST
}

// AstTaggedUnionPattern is the parse of:  tagged UnionName [ Pattern ]
type AstTaggedUnionPattern struct {
	TaggedUnionName     AST
	MemberPattern       AST    // nil if [ Pattern ] is absent
}

// ================================================================
// Top-level constructs in a package

// AstPackage is parse of 'package pkgStmt ... pkgStmt endpackage'
type AstPackage struct {
	PackageName   *AstIde
	PackageStmts  [] AST
}

// AstImport is parse of 'import x :: *;'
type AstImport struct {
	PackageName  *AstIde
}

// AstExport is parse of 'export x, y, z (..), w, ...;'
type AstExport struct {
	Ides         [] *AstIde
	WithMembers  [] bool
}

// AstImportBDPI is parse of 'import "BDPI" function_proto'
type AstImportBDPI struct {
	// TODO: C name of the function
	Proto  *AstFunctionProto
}

// AstTypedef is the parse of a BSV statement: "typedef typeDefinedAs newtype deriving (typeclass, ...);"
type AstTypedef struct {
	TypedefDefinee      *AstTypedefDefinee
	TypedefDefinedAs    AST
	TypeclassIdes       [] *AstIde
}

// AstIfcDecl is the parse of a top-level BSV declaration: 'interface ... endinterface'
type AstIfcDecl struct {
	Ifc                  *AstTypedefDefinee
        SubIfcOrMethodDecls  [] AST
}

// Sub-interface within an interface declaration
type AstIfcDeclSubIfcDecl struct {
	SubIfcName  *AstIde
	SubIfcType  AST
}

// Method declaration within an interface declaration
type AstIfcDeclMethodDecl struct {
	MethodName  *AstIde
	ReturnType  AST
	ArgNames    [] *AstIde
	ArgTypes    [] AST
}

//     instance typeclassIde # ( type { , type } ) [ provisos ] ;
//         { varAssign ; | functionDef | moduleDef }
//     endinstance [ : typeclassIde ]
type AstInstance struct {
	TypeclassIde  AST
	Types         [] AST
	Provisos      [] AST
	Stmts         [] AST
}

// ================================================================

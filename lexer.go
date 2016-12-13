// Lexical analyzer for BSV files

package goParseBSV

import (
	"bufio"        // Buffered I/O
	"fmt"
	"os"
	"io"
	"strings"
	"strconv"
	"utils"
)

// ================================================================
// Tokens

// The different types of lexical tokens.
const (
	TokNone  uint = iota
	TokEof
	TokKeyword
	TokIde          // identifiers
	TokInteger
	TokString
	TokOther
	TokUnknown
)

type Token struct {
	TokType    uint
	StringVal  string
	IntVal     int64    // original string is in 'StringVal' field
	IntWidth   int      // BSV bit-width; -1 for unspecified
	LineNum    int
	Column     int
}

func (tok *Token) PrintToken (fout *os.File) {
	fmt.Fprintf (fout, "Token [L %0d C %0d]: ", tok.LineNum, tok.Column)
	switch tok.TokType {
	case TokNone:        fmt.Fprintf (fout, "NONE")
	case TokEof:         fmt.Fprintf (fout, "EOF")
	case TokKeyword:     fmt.Fprintf (fout, "KEYWORD %s", tok.StringVal)
	case TokIde:         fmt.Fprintf (fout, "IDENTIFIER %s", tok.StringVal)
	case TokInteger: {
		fmt.Fprintf (fout, "INTEGER %q (= %d)", tok.StringVal, tok.IntVal)
		if (tok.IntWidth == -1) {
			fmt.Fprintf (fout, " Integer")
		} else {
			fmt.Fprintf (fout, " #(%0d)", tok.IntWidth)
		}
	}
	case TokString:      fmt.Fprintf (fout, "STRING %q", tok.StringVal)
	case TokOther:       fmt.Fprintf (fout, "OTHER %s", tok.StringVal);
	case TokUnknown:     fmt.Fprintf (fout, "UNKNOWN %s", tok.StringVal)
	default:             fmt.Fprintf (fout, "<UNKNOWN TokType %d>", tok.TokType)
	}
}

// Lexer objects encapsulate a lexer for a source file.
type Lexer struct {
	ifdefMacros   [] string            // for this file and nested `includes
	filename      string
	reader       *bufio.Reader
	prevLexer    *Lexer                // stack of nested `includes

	ch            byte
	line          string
	lineNum       int
	colNum        int
	ifdefStack    [] ifdefStackElem    // for nested `ifdef/ifndef-`elsif-`else-`endif

	Tracefile    *os.File              // for debugging
	Token        *Token
}

func printLocation (fout *os.File, lexer *Lexer, prefix string) () {
	fmt.Fprintf (fout, "%sFile %s [L %0d C %0d]\n", prefix, lexer.filename, lexer.lineNum, lexer.colNum)
	fmt.Fprintf (fout, "%sLine: %q\n", prefix, lexer.line)
}

func (lexer *Lexer) PrintToken (fout *os.File, prefix string) {
	fmt.Fprintf (fout, "%sToken: ", prefix)
	lexer.Token.PrintToken (fout)
	fmt.Fprintf (fout, "\n")
	fmt.Fprintf (fout, "%sLine: %q\n", prefix, lexer.line)
	fmt.Fprintf (fout, "%sFile: %s\n", prefix, lexer.filename)
}

// NewLexer creates a new lexer for the given file, macro defs, and debug trace file.
func NewLexer (inputFilename string, macros []string, ftrace *os.File) (*Lexer) {
	// Open the file for reading
	var fin *os.File
	var err  error
	if inputFilename == "-" {
		fin = os.Stdin
		fmt.Fprintf (os.Stdout, "Taking input from stdin\n")
	} else {
		fin, err = os.Open (inputFilename)
		if err != nil {
			fmt.Fprintf (os.Stderr, "ERROR: unable to open file %q for reading\n", inputFilename)
			fmt.Fprintf (os.Stderr, "%v\n", err)
			os.Exit (1)
		}
		fmt.Fprintf (os.Stdout, "Taking input from file %q\n", inputFilename)
	}

	var lexer = new (Lexer)
	lexer.ifdefMacros    = macros
	lexer.filename       = inputFilename
	lexer.reader         = bufio.NewReader (fin)
	lexer.prevLexer      = nil

	lexer.ch             = ' '
	lexer.line           = ""
	lexer.lineNum        = 0
	lexer.colNum         = 0
	lexer.ifdefStack     = make ([] ifdefStackElem, 1, 10)
	lexer.ifdefStack [0] = ifdefStackElem {ifClauseSatisfied: true,
		thisClauseActive: true,
		inElse: false,
		currentMacro: "ALWAYS_DEFINED"}
	getchar (lexer, true)        // get the first char; eof is ok (file may be empty)

	lexer.Tracefile  = ftrace    // output all tokens to this file
	lexer.Token      = nil
	return lexer
}

// pushLexer is called on a `include.
// Creates a new lexer and pushes the current lexer on the prevLexer stack.
func pushLexer (inputFilename string, lexer *Lexer) () {

	// Open the file for reading
	fin, err := os.Open (inputFilename)
	if err != nil {
		fmt.Fprintf (os.Stderr, "Unable to open file %s for reading\n", inputFilename)
		fmt.Fprintf (os.Stderr, "%v\n", err)
		os.Exit (1)
	}
	fmt.Fprintf (os.Stdout, "Taking input from included file: %q\n", inputFilename)

	var pushedLexer = new (Lexer)

	// Copy contents of lexer into pushedLexer
	*pushedLexer = *lexer

	// Make lexer point at pushedLexer, i.e., it's a new top-of-stack of lexers
	lexer.prevLexer      = pushedLexer

	// Re-initialize lexer on the new, included input file
	lexer.filename       = inputFilename
	lexer.reader         = bufio.NewReader (fin)

	lexer.ch             = ' '
	lexer.line           = ""
	lexer.lineNum        = 0
	lexer.colNum         = 0
	lexer.ifdefStack     = make ([] ifdefStackElem, 1, 10)
	lexer.ifdefStack [0] = ifdefStackElem {ifClauseSatisfied: true,
		thisClauseActive: true,
		inElse: false,
		currentMacro: "ALWAYS_DEFINED"}
	getchar (lexer, true)        // get the first char; eof is ok (file may be empty)

	lexer.Token      = nil
}

// popLexer is called at the end of a `include'd file.
// Restores the lexer to the `include parent from the prevLexer stack.
func popLexer (lexer *Lexer) () {
	pushedLexer := lexer.prevLexer
	fmt.Fprintf (os.Stdout, "Finished included file  : %q\n", lexer.filename)
	fmt.Fprintf (os.Stdout, "Resuming input from file: %q\n", pushedLexer.filename)

	// Copy contents of pushedLexer into lexer
	*lexer = *pushedLexer

	// We drop pushedLexer on the floor.
	// lexer.prevLexer now points to pushedLexer.prevLexer
}

// The ifdef stack is an array of ifdefStackElem's.
type ifdefStackElem struct {
	ifClauseSatisfied bool    // If any ifdef/ifndef/elsif was satisfied (therefore skip remaining elsif/endif)
	thisClauseActive  bool    // If lines in current arm are active or to be skipped
	inElse            bool    // If in else arm (therefore illegal to see elsif of else)
	currentMacro      string  // for error messages: current macro of ifdef/ifndef/elsif
}
	
func pushIfdefStack (lexer *Lexer, elem ifdefStackElem) () {
	lexer.ifdefStack = append (lexer.ifdefStack, elem)
	// fmt.Fprintf (os.Stdout, "pushIfdefStack: new stack is %v\n", lexer.ifdefStack)
}

func topOfIfdefStack (lexer *Lexer) (ifdefStackElem) {
	return lexer.ifdefStack [len (lexer.ifdefStack) - 1]
}

func popIfdefStack (lexer *Lexer) () {
	depth := len (lexer.ifdefStack)
	if (depth == 1) {
		fmt.Fprintf (os.Stderr, "ERROR: ifdef-stack underflow (too many endifs?)\n")
		printLocation (os.Stderr, lexer, "")
		os.Exit (1)
	}
	lexer.ifdefStack = lexer.ifdefStack [:depth - 1]
	// fmt.Fprintf (os.Stdout, "popIfdefStack: new stack %v\n", lexer.ifdefStack)
}

// readline reads an input line.
// Error if eofOk is false, or inside an `ifdef...`endif.
func readline (lexer *Lexer, eofOk bool) {
	lexer.line = ""
	for {
		nextCh, err := lexer.reader.ReadByte ()
		if err == io.EOF {
			if len (lexer.ifdefStack) > 1 {
				fmt.Fprintf (os.Stderr, "ERROR: EOF while inside the following ifdefs/ifndefs\n")
				for j := len (lexer.ifdefStack) - 1; j > 0; j-- {
					fmt.Fprintf (os.Stderr, "    %s\n", lexer.ifdefStack [j].currentMacro)
				}
				os.Exit (1)
			}
			if ! eofOk {
				fmt.Fprintf (os.Stderr, "ERROR: Unexpected EOF\n")
				printLocation (os.Stderr, lexer, "")
				os.Exit (1)
			}
			// If in a nested `include, pop to the parent lexer.
			if lexer.prevLexer != nil {
				popLexer (lexer)
				lexer.line = ""
				continue
			}
			lexer.line = "\0000"
			break
		} else if nextCh == '\n' {
			lexer.line += string (nextCh)
			break
		} else {
			lexer.line += string (nextCh)
		}
	}
	lexer.lineNum++
	lexer.colNum = 1
}

// checkIncludeCycle checks if there is a cycle in the `include chain,
// i.e., whether a file is effectively including itself through a chain of includes.
func checkIncludeCycle (includeFilename string, lexer *Lexer) (bool) {
	if lexer == nil {
		return false
	} else if includeFilename == lexer.filename {
		fmt.Fprintf (os.Stderr, "ERROR: Chain of `includes cycles back; ignoring this final `include\n")
		fmt.Fprintf (os.Stderr, "    ")
		printLocation (os.Stderr, lexer, "")
		return true
	} else {
		b := checkIncludeCycle (includeFilename, lexer.prevLexer)
		if b {
			fmt.Fprintf (os.Stderr, "    ")
			printLocation (os.Stderr, lexer, "")
		}
		return b
	}
}

// processIncludeLine handles lines that begin with `include.
// Push a new lexer for the included file.
func processIncludeLine (lexer *Lexer) () {
	trimmedLine := strings.TrimSpace (lexer.line)

	// Skip whitespace after `include
	var j int = len ("`include")
	for {
		if j >= len (trimmedLine) {
			fmt.Fprintf (os.Stderr, "ERROR: `include line syntax\n")
			printLocation (os.Stderr, lexer, "")
			os.Exit (1)
		}
		if (trimmedLine [j] != ' ') { break }
		j++
	}
	// Check for opening quote or <
	if (trimmedLine [j] != '"') && (trimmedLine [j] != '<') {
		fmt.Fprintf (os.Stderr, "ERROR: `include line syntax\n")
		printLocation (os.Stderr, lexer, "")
		os.Exit (1)
	}
	openQuote := trimmedLine [j]
	j++
	// Collect the file name
	includeFilename := ""
	for {
		if j >= len (trimmedLine) {
			fmt.Fprintf (os.Stderr, "ERROR: `include line syntax\n")
			printLocation (os.Stderr, lexer, "")
			os.Exit (1)
		}
		if (openQuote == '"') && (trimmedLine [j] == '"') { break }
		if (openQuote == '<') && (trimmedLine [j] == '>') { break }
		includeFilename += string (trimmedLine [j])
		j++
	}
	lexer.colNum = len (lexer.line)

	isCyclic := checkIncludeCycle (includeFilename, lexer)
	if isCyclic {
		return    // Ignore this include line
	}

	pushLexer (includeFilename, lexer)
}

// getMacroFromCurrentLine processes a line starting with `ifdef, `ifndef or `elsif.
// Return the macro name that follows it.
func getMacroFromCurrentLine (lexer *Lexer) (string) {
	fields := strings.Fields (lexer.line)
	if len (fields) != 2 {
		fmt.Fprintf (os.Stderr, "ERROR: Macro line (ifdef/ifndef/elseif) not well formed\n")
		fmt.Fprintf (os.Stderr, "    Must be followed by just the macro name\n")
		printLocation (os.Stderr, lexer, "")
		os.Exit (1)
	}
	return fields [1]
}

// macroIsDefined processes a line starting with `ifdef, `ifndef or `elsif.
// Check if the macro that follows is defined.
func macroIsDefined (lexer *Lexer) (bool) {
	thisMacro := getMacroFromCurrentLine (lexer)
	defined := false
	for _, definedMacro := range lexer.ifdefMacros {
		if definedMacro == thisMacro {
			defined = true
			break
		}
	}
	return defined
}

// readLineModuloIfdefs reads a line that is active w.r.t., `ifdef/ifndef-`elsif-`else-`endif.
func readLineModuloIfdefs (lexer *Lexer, eofOk bool) () {
	for {
		readline (lexer, eofOk)
		trimmedLine := strings.TrimSpace (lexer.line)

		if strings.HasPrefix (trimmedLine, "`ifdef") {
			elemTop := topOfIfdefStack (lexer)
			macro := getMacroFromCurrentLine (lexer)
			if macroIsDefined (lexer) {
				pushIfdefStack (lexer, ifdefStackElem {true, elemTop.thisClauseActive, false, macro})
			} else {
				pushIfdefStack (lexer, ifdefStackElem {false, false, false, macro})
			}

		} else if strings.HasPrefix (trimmedLine, "`ifndef") {
			elemTop := topOfIfdefStack (lexer)
			macro := getMacroFromCurrentLine (lexer)
			if macroIsDefined (lexer) {
				pushIfdefStack (lexer, ifdefStackElem {false, false, false, macro})
			} else {
				pushIfdefStack (lexer, ifdefStackElem {true, elemTop.thisClauseActive, false, macro})
			}

		} else if strings.HasPrefix (trimmedLine, "`elsif") {
			elemTop := topOfIfdefStack (lexer)
			if elemTop.inElse {
				fmt.Fprintf (os.Stderr, "ERROR: 'elsif' not allowed after 'else'\n")
				printLocation (os.Stderr, lexer, "")
				os.Exit (1)
			}
			macro := getMacroFromCurrentLine (lexer)
			popIfdefStack (lexer)
			elemNext := topOfIfdefStack (lexer)
			if elemTop.ifClauseSatisfied {
				pushIfdefStack (lexer, ifdefStackElem {true, false, false, macro})
			} else if macroIsDefined (lexer) {
				pushIfdefStack (lexer, ifdefStackElem {true, elemNext.thisClauseActive, false, macro})
			} else {
				pushIfdefStack (lexer, ifdefStackElem {false, false, false, macro})
			}

		} else if strings.HasPrefix (trimmedLine, "`else") {
			elemTop := topOfIfdefStack (lexer)
			if elemTop.inElse {
				fmt.Fprintf (os.Stderr, "ERROR: 'else' not allowed after 'else'\n")
				printLocation (os.Stderr, lexer, "")
				os.Exit (1)
			}
			macro   := elemTop.currentMacro
			popIfdefStack (lexer)
			elemNext := topOfIfdefStack (lexer)
			if elemTop.ifClauseSatisfied {
				pushIfdefStack (lexer, ifdefStackElem {true, false, true, macro})
			} else {
				pushIfdefStack (lexer, ifdefStackElem {true, elemNext.thisClauseActive, true, macro})
			}

		} else if strings.HasPrefix (trimmedLine, "`endif") {
			popIfdefStack (lexer)

		} else if topOfIfdefStack (lexer).thisClauseActive {
			if strings.HasPrefix (trimmedLine, "`include") {
				processIncludeLine (lexer)
			} else {
				// An actual source line
				return
			}
		} else {
			// ! thisClauseActive; skip this line
		}
	}
}

// getchar gets the next char from the input file, modulo ifdefs.
func getchar (lexer *Lexer, eofOk bool) () {
	lexer.colNum++

	// if gone past end of last line, read in another line
	if lexer.colNum > len (lexer.line) {
		readLineModuloIfdefs (lexer, eofOk)
	}
	lexer.ch = lexer.line [lexer.colNum - 1]
}

// getDigitString parses a digit string in base 2, 8, 10 or 16
// returning original string in lexer.Token.StringVal,
// value in IntVal
func getDigitString (lexer *Lexer, base int) () {
	// Invariant: lexer.ch is first digit (or spacer '_') of a digit string
	lexer.Token.StringVal += string (lexer.ch)
	buffer := ""
	if (lexer.ch != '_') {
		buffer += string (lexer.ch)
	}
	getchar (lexer, true)
	for {
		if lexer.ch == '_' {
			lexer.Token.StringVal += string (lexer.ch)
			getchar (lexer, true)
		} else if (base == 10) && utils.ByteIsDigit (lexer.ch) {
			lexer.Token.StringVal += string (lexer.ch)
			buffer += string (lexer.ch)
			getchar (lexer, true)
		} else if (base == 16) && utils.ByteIsHexdigit (lexer.ch) {
			lexer.Token.StringVal += string (lexer.ch)
			buffer += string (lexer.ch)
			getchar (lexer, true)
		} else if (base == 8) && utils.ByteIsOctdigit (lexer.ch) {
			lexer.Token.StringVal += string (lexer.ch)
			buffer += string (lexer.ch)
			getchar (lexer, true)
		} else if (base == 2) && utils.ByteIsBindigit (lexer.ch) {
			lexer.Token.StringVal += string (lexer.ch)
			buffer += string (lexer.ch)
			getchar (lexer, true)
		} else {
			break
		}
	}
	n, err := strconv.ParseInt (buffer, base, 64)
	if (err != nil) {
		fmt.Fprintf (os.Stderr, "ERROR: lexer: parsing integer\n")
		printLocation (os.Stderr, lexer, "")
		fmt.Fprintf (os.Stderr, "    buffer %q base %0d\n", buffer, base)
		os.Exit (1)
	} else {
		lexer.Token.IntVal = n
	}
}

// GetToken gets the next token from a file, modulo ifdefs.
func GetToken (lexer *Lexer) () {
	if ((lexer.Token != nil) && (lexer.Token.TokType == TokEof)) {
		return
	}

	// Skip whitespace and comments
	for {
		if utils.ByteIsWhitespace (lexer.ch) {
			getchar (lexer, true)
		} else if (lexer.ch == 0) {
			break
		} else if lexer.ch == '/' {
			lexer.Token = new (Token)
			lexer.Token.TokType = TokOther
			lexer.Token.StringVal = "/"
			lexer.Token.LineNum    = lexer.lineNum
			lexer.Token.Column     = lexer.colNum
			getchar (lexer, false)
			if lexer.ch == '/' {
				// Start of a '//' comment
				for {
					getchar (lexer, true)
					if (lexer.ch == 0) {
						fmt.Printf ("WARNING: '//' comment ends in end-of-file\n")
						printLocation (os.Stdout, lexer, "")
						break
					} else if lexer.ch == '\n' {
						getchar (lexer, true)
						break
					}
				}
			} else if lexer.ch == '*' {
				// Start of a '/*' comment
				for {
					getchar (lexer, true)
					if lexer.ch == 0 {
						fmt.Printf ("WARNING: '/*' comment ends in end-of-file\n")
						printLocation (os.Stdout, lexer, "")
						break
					} else if lexer.ch == '*' {
						getchar (lexer, false)
						if lexer.ch == '/' {
							getchar (lexer, true)
							break
						}
					}
				}
			} else {
				return
			}
		} else {
			break
		}
	}
	// Invariant: lexer.ch has been read, is not whitespace, is not in comment

	lexer.Token = new (Token)
	lexer.Token.StringVal = ""
	lexer.Token.LineNum    = lexer.lineNum
	lexer.Token.Column     = lexer.colNum

	switch {
	case lexer.ch == 0: {
		lexer.Token.TokType = TokEof
	}
	case utils.ByteIsAlpha (lexer.ch) || (lexer.ch == '$') || (lexer.ch == '_'): {
		for {
			lexer.Token.StringVal += string (lexer.ch)
			getchar (lexer, true)
			if utils.ByteIsAlpha (lexer.ch) ||
				(lexer.ch == '_') ||
				utils.ByteIsDigit (lexer.ch) {
			} else {
				break
			}
		}
		if isKeyword (lexer.Token.StringVal) {
			lexer.Token.TokType = TokKeyword
		} else {
			lexer.Token.TokType = TokIde
		}
	}
	case utils.ByteIsDigit (lexer.ch): {
		lexer.Token.TokType  = TokInteger
		lexer.Token.IntWidth = -1    // unspecified width
		getDigitString (lexer, 10)

		if lexer.ch == '\'' {
			lexer.Token.StringVal += string (lexer.ch)
			lexer.Token.IntWidth = int (lexer.Token.IntVal)

			getchar (lexer, false)
			var base int
			switch lexer.ch {
			case 'h': { base = 16 }
			case 'd': { base = 10 }
			case 'o': { base =  8 }
			case 'b': { base =  2 }
			default: {
				fmt.Fprintf (os.Stderr, "Unrecognized base %c in integer literal\n", lexer.ch)
				printLocation (os.Stderr, lexer, "")
				os.Exit (1)
			}
			}
			lexer.Token.StringVal += string (lexer.ch)

			getchar (lexer, true)
			getDigitString (lexer, base)
		}
	}
	case lexer.ch == '\'': {
		lexer.Token.TokType = TokInteger
		lexer.Token.IntWidth = -1    // unspecified width
		lexer.Token.StringVal = string (lexer.ch)

		getchar (lexer, false)

		if lexer.ch == '1' {
			lexer.Token.StringVal += string (lexer.ch)
			lexer.Token.IntVal = -1
			getchar (lexer, true)
		} else {
			var base int
			switch lexer.ch {
			case 'h': { base = 16 }
			case 'd': { base = 10 }
			case 'o': { base =  8 }
			case 'b': { base =  2 }
			default: {
				fmt.Fprintf (os.Stderr, "Unrecognized base %c in integer literal\n", lexer.ch)
				printLocation (os.Stderr, lexer, "")
				os.Exit (1)
			}
			}
			lexer.Token.StringVal += string (lexer.ch)

			getchar (lexer, true)
			getDigitString (lexer, base)
		}
	}
	case lexer.ch == '"': {
		lexer.Token.TokType = TokString
		lexer.Token.StringVal = ""
		getchar (lexer, false)
		for {
			if lexer.ch == '"' {
				getchar (lexer, true)
				break
			} else if lexer.ch == '\\' {
				getchar (lexer, false)
				switch lexer.ch {
				case '\\', '"': { lexer.Token.StringVal += string (lexer.ch) }
				case 'n': { lexer.Token.StringVal += "\n" }
				case 'r': { lexer.Token.StringVal += "\r" }
				case 't': { lexer.Token.StringVal += "\t" }
				default:  {
					fmt.Fprintf (os.Stdout,
						"WARNING: Unrecognized \\-escaped char '%c' (= %0d), taking it as-is\n",
						lexer.ch,
						lexer.ch)
					printLocation (os.Stdout, lexer, "")
					lexer.Token.StringVal += string (lexer.ch)
				}
				}
				getchar (lexer, false)
			} else {
				lexer.Token.StringVal += string (lexer.ch)
				getchar (lexer, false)
			}
		}
	}
	case strings.IndexByte ("{})[]#$;,`'?-+%", lexer.ch) != -1: {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = string (lexer.ch)
		getchar (lexer, true)
	}
	case lexer.ch == '.': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "."
		getchar (lexer, true)
		if lexer.ch == '.' {
			lexer.Token.StringVal = ".."
			getchar (lexer, true)
		}
	}
	case lexer.ch == '*': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "*"
		getchar (lexer, true)
		if lexer.ch == '*' {
			lexer.Token.StringVal = "**"
			getchar (lexer, true)
		} else if lexer.ch == ')' {
			lexer.Token.StringVal = "*)"
			getchar (lexer, true)
		}
	}
	case lexer.ch == ':': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = ":"
		getchar (lexer, true)
		if lexer.ch == ':' {
			lexer.Token.StringVal = "::"
			getchar (lexer, true)
		}
	}
	case lexer.ch == '(': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "("
		getchar (lexer, true)
		if lexer.ch == '*' {
			lexer.Token.StringVal = "(*"
			getchar (lexer, true)
		}
	}
	case lexer.ch == '<': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "<"
		getchar (lexer, true)
		if lexer.ch == '<' {
			lexer.Token.StringVal = "<<"
			getchar (lexer, true)
		} else if lexer.ch == '=' {
			lexer.Token.StringVal = "<="
			getchar (lexer, true)
		} else if lexer.ch == '-' {
			lexer.Token.StringVal = "<-"
			getchar (lexer, true)
		}
	}
	case lexer.ch == '>': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = ">"
		getchar (lexer, true)
		if lexer.ch == '=' {
			lexer.Token.StringVal = ">="
			getchar (lexer, true)
		} else if lexer.ch == '>' {
			lexer.Token.StringVal = ">>"
			getchar (lexer, true)
		}
	}
	case lexer.ch == '=': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "="
		getchar (lexer, true)
		if lexer.ch == '=' {
			lexer.Token.StringVal = "=="
			getchar (lexer, true)
		}
	}
	case lexer.ch == '!': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "!"
		getchar (lexer, true)
		if lexer.ch == '=' {
			lexer.Token.StringVal = "!="
			getchar (lexer, true)
		}
	}
	case lexer.ch == '&': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "&"
		getchar (lexer, true)
		if lexer.ch == '&' {
			lexer.Token.StringVal = "&&"
			getchar (lexer, true)
			if lexer.ch == '&' {
				lexer.Token.StringVal = "&&&"
				getchar (lexer, true)
			}
		}
	}
	case lexer.ch == '|': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "|"
		getchar (lexer, true)
		if lexer.ch == '|' {
			lexer.Token.StringVal = "||"
			getchar (lexer, true)
		}
	}
	case lexer.ch == '^': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "^"
		getchar (lexer, true)
		if lexer.ch == '~' {
			lexer.Token.StringVal = "^~"
			getchar (lexer, true)
		}
	}
	case lexer.ch == '~': {
		lexer.Token.TokType = TokOther
		lexer.Token.StringVal = "~"
		getchar (lexer, true)
		if lexer.ch == '|' {
			lexer.Token.StringVal = "~|"
			getchar (lexer, true)
		} else if lexer.ch == '&' {
			lexer.Token.StringVal = "~&"
			getchar (lexer, true)
		} else if lexer.ch == '^' {
			lexer.Token.StringVal = "~^"
			getchar (lexer, true)
		}
	}
	default: {
		fmt.Fprintf (os.Stderr, "ERROR: lexer: unrecognized character '%c' = (%0d)\n", lexer.ch, lexer.ch)
		printLocation (os.Stderr, lexer, "")
		os.Exit (1)
	}
	}

	// Debugging
	if lexer.Tracefile != nil {
		lexer.Token.PrintToken (lexer.Tracefile)
		fmt.Fprintf (lexer.Tracefile, "\n")
	}
}

// ================================================================
// Keywords
// TODO: sort alphabetically and use binary search


var keywordTable = [...] string {
	"BVI",
	"C",
	"CF",
	"E",
	"SB",
	"SBR",
	"action",
	"actionvalue",
	"ancestor",
	"begin",
	"break",
	"case",
	"clocked_by",
	"continue",
	"default",
	"default_clock",
	"default_reset",
	"dependencies",
	"deriving",
	"determines",
	"else",
	"enable",
	"end",
	"endaction",
	"endactionvalue",
	"endcase",
	"endfunction",
	"endinstance",
	"endinterface",
	"endmethod",
	"endmodule",
	"endpackage",
	"endpar",
	"endrule",
	"endrules",
	"endseq",
	"endtypeclass",
	"enum",
	"export",
	"for",
	"function",
	"if",
	"ifc_inout",
	"import",
	"inout",
	"input_clock",
	"input_reset",
	"instance",
	"interface",
	"let",
	"match",
	"matches",
	"method",
	"module",
	"numeric",
	"output_clock",
	"output_reset",
	"package",
	"parameter",
	"par",
	"path",
	"port",
	"provisos",
	"ready",
	"reset_by",
	"return",
	"rule",
	"rules",
	"same_family",
	"schedule",
	"seq",
	"struct",
	"tagged",
	"type",
	"typeclass",
	"typedef",
	"union",
	"void",
	"while" }

func isKeyword (s string) (bool) {
	for _, kw := range (keywordTable) {
		if s == kw { return true }
	}
	return false
}

func MatchingEndKeyword (kw string) (string) {
	var endkw string
	switch (kw) {
	case "action":      endkw = "endaction"
	case "actionvalue": endkw = "endactionvalue"
	case "begin":       endkw = "end"
	case "case":        endkw = "endcase"
	case "function":    endkw = "endfunction"
	case "instance":    endkw = "endinstance"
	case "interface":   endkw = "endinterface"
	case "method":      endkw = "endmethod"
	case "module":      endkw = "endmodule"
	case "package":     endkw = "endpackage"
	case "rule":        endkw = "endrule"
	case "rules":       endkw = "endrules"
	case "typeclass":   endkw = "endtypeclass"
	default: {
		fmt.Fprintf (os.Stderr, "ERROR: MatchingEndKeyword (\"%s\"): unknown begin-keyword\n", kw)
		os.Exit (1)
	}
	}
	return endkw
}

// ================================================================

// TestLexer read tokens from a file and print them to stdout.
func TestLexer () () {
	macros, inputFilename := ParseCommandLine ()
	lexer := NewLexer (inputFilename, macros, nil)
	for {
		GetToken (lexer)
		var tok *Token = lexer.Token
		tok.PrintToken (os.Stdout)
		fmt.Printf ("\n")
		if tok.TokType == TokEof { break }
	}
}

// ================================================================

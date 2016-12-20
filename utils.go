// Misc. utility functions to classify characters

package goParseBSV

import (
	"strings"
)

// ================================================================
// Help functions

func ByteIsWhitespace (x byte) bool {
	return strings.IndexByte (" \n\t\r", x) != -1
}

func ByteIsLower (x byte) bool {
	return ('a' <= x) && (x <= 'z')
}

func ByteIsUpper (x byte) bool {
	return ('A' <= x) && (x <= 'Z')
}

func ByteIsAlpha (x byte) bool {
	return ByteIsLower (x) || ByteIsUpper (x)
}

func ByteIsDigit (x byte) bool {
	return strings.IndexByte ("0123456789", x) != -1
}

func ByteIsHexdigit (x byte) bool {
	return strings.IndexByte ("0123456789ABCDEFabcdef", x) != -1
}

func ByteIsOctdigit (x byte) bool {
	return strings.IndexByte ("012345678", x) != -1
}

func ByteIsBindigit (x byte) bool {
	return strings.IndexByte ("01", x) != -1
}

// ================================================================

--- This library provides basic support for UTF-8 encoding.
-- @module utf8

local utf8 = {}

---
-- Receives zero or more integers, converts each one to its corresponding UTF-8 byte sequence and returns
-- a string with the concatenation of all these sequences.
function utf8.char (...) end

---
-- The pattern  "[\0-\x7F\xC2-\xF4][\x80-\xBF]*" , which matches exactly one
-- UTF-8 byte sequence, assuming that the subject is a valid UTF-8 string.
-- @field charpattern

---
-- Iterate over all characters in string.
--
--      for p, c in utf8.codes(s) do body end
--
-- will iterate over all characters in string s, with p being the position (in bytes) and c the code point
-- of each character. It raises an error if it meets any invalid byte sequence.
function utf8.codes (s) end

---
-- Returns the codepoints (as integers) from all characters in s that start between byte position i and j (both included).
-- The default for i is 1 and for j is i. It raises an error if it meets any invalid byte sequence.
function utf8.codepoint (s , i , j) end

---
-- Returns the number of UTF-8 characters in string s that start between positions i and j (both inclusive).
-- The default for i is 1 and for j is -1. If it finds any invalid byte sequence, returns a false value plus
-- the position of the first invalid byte.
function utf8.len (s , i , j) end

---
-- Returns the position (in bytes) where the encoding of the n-th character of s (counting from position i) starts.
-- A negative n gets characters before position i. The default for i is 1 when n is non-negative
-- and #s + 1 otherwise, so that utf8.offset(s, -n) gets the offset of the n-th character from the end
-- of the string.
-- If the specified character is neither in the subject nor right after its end, the function returns nil.
--
-- As a special case, when n is 0 the function returns the start of the encoding of the character that contains the i-th byte of s.
--
-- This function assumes that s is a valid UTF-8 string.
function utf8.offset (s, n , i) end

return utf8

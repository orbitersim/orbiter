--- manipulating Lua tables.
-- @module table

local table = {}

---
-- Given an array where all elements are strings or numbers, returns
-- `table[i]..sep..table[i+1] ... sep..table[j]`. The default value for
-- `sep` is the empty string, the default for `i` is 1, and the default for
-- `j` is the length of the table. If `i` is greater than `j`, returns the
-- empty string.
function table.concat(table , sep , i , j) end

---
-- Inserts element `value` at position `pos` in `table`, shifting up
-- other elements to open space, if necessary. The default value for `pos` is
-- `n+1`, where `n` is the length of the table (see §2.5.5), so that a call
-- `table.insert(t,x)` inserts `x` at the end of table `t`.
function table.insert(table, pos, value) end

---
-- Removes from `table` the element at position `pos`, shifting down other
-- elements to close the space, if necessary. Returns the value of the removed
-- element. The default value for `pos` is `n`, where `n` is the length of the
-- table, so that a call `table.remove(t)` removes the last element of table
-- `t`.
function table.remove(table , pos) end

---
-- Returns a new table with all parameters stored into keys 1, 2, etc. and with a field "n" with
-- the total number of parameters. Note that the resulting table may not be a sequence.
function table.pack (...) end
---
-- Sorts table elements in a given order,
-- *in-place*, from `table[1]` to `table[n]`, where `n` is the length of the
-- table. If `comp` is given, then it must be a function that receives two
-- table elements, and returns true when the first is less than the second
-- (so that `not comp(a[i+1],a[i])` will be true after the sort). If `comp`
-- is not given, then the '<' operator will be used.
function table.sort(table , comp) end

-- luacheck: ignore 121

---
-- Returns the elements from the given table. This function is equivalent to
--   return list[i], list[i+1], ..., list[j]
-- except that the above code can be written only for a fixed number of
-- elements. By default, `i` is 1 and `j` is the length of the list, as
-- defined by the length operator (see §2.5.5).
function unpack(list , i , j) end

return table

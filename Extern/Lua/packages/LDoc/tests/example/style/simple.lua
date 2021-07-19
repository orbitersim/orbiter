---------------
-- Markdown-flavoured and very simple no-structure style.
--
-- Here the idea is to structure the document entirely with [Markdown]().
--
-- Using the default markdown processor can be a little irritating: you are
-- required to give a blank line before starting lists. The default stylesheet
-- is not quite right, either.
--
module 'mod'

--- Combine two strings _first_ and _second_ in interesting ways.
function combine(first,second)
end

--- Combine a whole bunch of strings.
function combine_all(...)
end

---
-- Creates a constant capture. This pattern matches the empty string and
-- produces all given values as its captured values.
function lpeg.Cc([value, ...]) end


--- Split a string _str_. Returns the first part and the second part, so that
-- `combine(first,second)` is equal to _s_.
function split(s)
end

--- Split a string _text_ into a table.
-- Returns:
--
-- - `name` the name of the text
-- - `pairs` an array of pairs
--    - `key`
--    - `value`
--  - `length`
--

function split_table (text)
end

--- A table of useful constants.
--
-- - `alpha` first correction factor
-- - `beta` second correction factor
--
-- @table constants



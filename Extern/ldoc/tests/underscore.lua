--[[-----------
 testing underscores and Markdown pre-formatting.

 A list may immediately follow a line, if it is indented. Necessary lines
 will also be inserted after resuming current indentation.
  - a list
     - sublist (note the needed indentation for Markdown to recognize this)
     - more
  * of items
  more than one line
  - indeed
  can keep going
Inline _see references_: look at @{_ONE_} and @{table.concat}.
Indented code blocks may also follow immediately.
    function test(a,b)
       return three(a,b)
    end
 @module underscore
]]

----------
-- A function.
-- Can also use @{_TWO_ |second function}
function _ONE_() end

-----------
-- another function
-- @see string.format
function _TWO_() end

------
-- a longer summary.
-- @param a
-- @param b
-- @return one
-- - one-1
-- - one-2
-- @return two
function three(a,b) end

-----------
-- Main module.
-- This contains four functions, two of which are in
-- another section implicitly specified using the 'within' tag.
-- (This allows functions placed together to be classified
-- separately)
--
-- Furthermore, the 'mylib.extra' functions will be added
-- as their own section, allowing you to document a logical module
-- spanning several files
--
-- @module mylib

--- first.
function A ()

end

--- second, within its own section.
-- @within Utilities
function B ()

end

--- third.
function C ()

end

--- fourth, together with second.
-- @within Utilities
function D ()

end


------------
-- A new-style module.
-- Shows how @alias can be used to tell ldoc that a given name
-- is a shorthand for the full module name
-- @alias M

local simple_alias = {}
local M = simple_alias

--- return the answer. And complete the description
function M.answer()
  return 42
end

return simple_alias


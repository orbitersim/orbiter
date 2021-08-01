------------
-- A module containing tables.
-- Shows how Lua table definitions can be conveniently parsed.
--
-- There may be multiple comment lines per field/parameter, and
-- such comments may begin with `TYPE:`
--
-- Functions also can be commented in a similar way, and the last
-- parameter's comment may be outside the parens.
--
-- @alias M

local tables = {}
local M = tables

--- a function.
function M.one(
    bonzo, -- dog
           -- has its day!
    frodo) --baggins
end

--- first table.
-- @table one
M.one = {
    A = 1, -- alpha
    B = 2; -- beta
}

--- second table.
-- we don't need an explicit table tag, since it
-- can be inferred from the context.
M.two = {
    N = 10,  -- int: no. of cases
    L = 'label' -- string: case label
}

return M


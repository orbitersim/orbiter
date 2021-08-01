--- this module has a comment.

local local_two

--- a local function
local function local_one ()
end

--- a local function, needing explicit tag.
-- @local here
function local_two ()

end

--- A problem function.
-- @param p a parameter
function problem.fun(p)
    return 42
end

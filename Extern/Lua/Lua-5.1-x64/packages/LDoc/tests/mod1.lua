------
-- always need a doc comment to start!
-- Can have a module with no internal doc comments,
-- although you will get a warning. At least we no
-- longer get a 'end-of-file' if there is no explicit
-- module name.

----- not a doc comment -----
-- a common style when just specifying an informative comment
-- May start with a doc comment but has trailing hyphens

local g -- so g below must be marked as local

--- simple.
--@param x a parameter
function _M.f(x) end

--- implicit local function.
-- Local functions appear in dump but only in docs if you say --all
local function L(t,v) end

--- explicit local function.
-- @local here
function g(a,b) end

--- a table of this module
_M.contents = {
    A = 'f', -- alpha
    B = 'g'  -- beta
}

--- another way to do parameters.
function _M.kay(
    a, -- ay
    b, -- bee
) end

--- a field of this module.
_M.constant = 'hello'

--- functions can also be like so.
_M.why = function(x,y)
end




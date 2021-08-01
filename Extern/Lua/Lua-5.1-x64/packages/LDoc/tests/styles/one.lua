--[[
A non-doc comment
multi-line
probably containing license information!
Doesn't use module(), but module name is inferred from file name.
If you have initial licence comments that look like doc comments,
then set `boilerplate=true`
]]
------------
-- Test module,
-- Actual blurb here!
----

local one = {}

--- answer to everything.
function one.answer ()
    return 42
end

return one



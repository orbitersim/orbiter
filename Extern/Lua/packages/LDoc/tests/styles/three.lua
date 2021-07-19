------------
-- Alternative to no-magic style.
-- Description here
----

--- documented, but private
local function question ()
end

--- answer to everything.
-- @return magic number
local function answer ()
    return 42
end

--- @export
return {
    answer = answer
}



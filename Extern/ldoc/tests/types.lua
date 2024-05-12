-----------------------
-- Module using tparam for typed parameters.
--
-- @module types

--- has typed parameters, `string` and `int`.
-- And never forget `E = m*c^2`.
-- Reference to `two.md.First`
--
-- A reference to `mydata`.
-- @string name
-- @int age
-- @treturn mydata
function _M.first (name,age)

end

--- simple function returning something
-- @return bonzo dog!
function _M.simple()

end


--- a table of this module.
-- @table mydata
_M.mydata = {
    one = 1, -- alpha
    two = 2, -- beta
}





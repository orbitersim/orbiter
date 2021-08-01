-------
-- @module bad

local bad = {}

--------
-- inference fails! Have to explicitly
-- declare the function and its arguments
bad['entry'] = function(one)
end

return bad


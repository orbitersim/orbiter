------------
-- A little old-style module
local io = io
-- we'll look for this
module 'simple'

-- if it were 'module (...)' then the name has to be deduced.

--- return the answer.
function answer()
  return 42
end

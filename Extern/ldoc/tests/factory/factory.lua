---
-- Useful classes.
-- This is the enclosing module description.

--- My class.
-- Describe our class
-- @factory Object

local make_object
do
  --- my private method
  -- document here. (By default it will not show in docs.)
  -- @private
  local my_private_method = function(self)
    ...more code here...
  end

  --- my public method.
  -- documentation here
  -- @param arg
  local method = function(self, arg)
     .....some code here.....
     return my_private_method(self)
  end

   --- Another public method.
   -- More details
   local more = function(self)
   end

  --- factory returning @{Object}.
  -- @constructor
  -- @param arg
  -- @param arg2
  make_object = function(arg, arg2)
    return
    {
      -- private fields
      field_ = arg;

      -- public methods
      method = method;
      more = more;
    }
  end
end

return {
    make_object = make_object
}


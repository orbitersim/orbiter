--------------------------------------------------------------------------------
--- Queue of objects sorted by priority.
-- @module lua-nucleo.priority_queue
-- This file is a part of lua-nucleo library. Note that if you wish to spread
-- the description after tags, then invoke with `not_luadoc=true`. 
-- The flags here are `ldoc -X -f backtick priority_queue.lua`, which
-- also expands backticks.
-- @copyright lua-nucleo authors (see file `COPYRIGHT` for the license)
--------------------------------------------------------------------------------

local arguments,
      method_arguments
      = import 'lua-nucleo/args.lua'
      {
        'arguments',
        'method_arguments'
      }

local lower_bound_gt
      = import 'lua-nucleo/algorithm.lua'
      {
        'lower_bound_gt'
      }

--------------------------------------------------------------------------------

local table_insert, table_remove = table.insert, table.remove

--------------------------------------------------------------------------------

--- Priority Queue
-- @factory priority_queue
local make_priority_queue
do
  local PRIORITY_KEY = 1
  local VALUE_KEY = 2

  local insert = function(self, priority, value)
    method_arguments(
        s
        "number", priority
      )
    assert(value ~= nil, "value can't be nil") -- value may be of any type, except nil

    local queue = self.queue_
    local k = lower_bound_gt(queue, PRIORITY_KEY, priority)

    table_insert(queue, k, { [PRIORITY_KEY] = priority, [VALUE_KEY] = value })
  end

  local front = function(self)
    method_arguments(
        self
      )

    local queue = self.queue_
    local front_elem = queue[#queue]

    if front_elem == nil then
      return nil
    end

    return front_elem[PRIORITY_KEY], front_elem[VALUE_KEY]
  end

  --- pop last value.
  -- @return priority
  -- @return value
  local pop = function(self)
    method_arguments(
        self
      )

    local front_elem = table_remove(self.queue_)

    if front_elem == nil then
      return nil
    end

    return front_elem[PRIORITY_KEY], front_elem[VALUE_KEY]
  end

  --- construct a `priority_queue`.
  -- @constructor
  make_priority_queue = function()
    --- @export
    return
    {
      insert = insert;
      front = front;
      pop = pop;
      --
      queue_ = { };
    }
  end
end

return
{
  make_priority_queue = make_priority_queue;
}

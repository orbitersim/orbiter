--[[--------
A simple module with examples.

Even without markdown formatting, blank lines are respected.

@module usage
]]

local usage = {}

local helper

--- a local helper function.
-- @local
function helper ()
end

----------
-- A simple vector class.
--
-- Supports arithmetic operations.
-- @usage
-- v = Vector.new {10,20,30}
-- assert (v == Vector{10,20,30})
-- @type Vector

local Vector = {}
usage.Vector = {}


----------
-- Create a vector from an array `t`.
-- `Vector` is also callable!
function Vector.new (t)
end

-- note that @function may have modifiers. Currently
-- we aren't doing anything with them, but LDoc no longer
-- complains (issue #45). Note also that one needs
-- explicit @param tags with explicit @function; 'static'
-- methods must have a @constructor or a @static tag.

----------
-- Create a vector from a string.
-- @usage
--  v = Vector.parse '[1,2,3]'
--  assert (v == Vector.new {1,2,3})
-- @function[kind=ctor] parse
-- @static
-- @param s
function Vector.parse (s)
end

--------
-- Compare two vectors for equality.
function Vector:__eq (v)
end

----------
-- Add another vector, array or scalar `v` to this vector.
-- Returns new `Vector`
-- @usage assert(Vector.new{1,2,3}:add(1) == Vector{2,3,4})
function Vector:add (v)
end

----------
-- set vector options. `opts` is a `Vector.Opts` table.
function Vector:options (opts)
end

--[[-----------------
@table Vector.Opts
Options table format for `Vector:options`

 * `autoconvert`:  try to convert strings to numbers
 * `adder`: function used to perform addition and subtraction
 * `multiplier`: function used to perform multiplication and division

@usage
    v = Vector {{1},{2}}
    v:options {adder = function(x,y) return {x[1]+y[1]} end}
    assert(v:add(1) == Vector{{2},{3}})
]]

return usage



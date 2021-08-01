------------
-- Yet another module.
-- Description can continue after simple tags, if you
-- like - but to keep backwards compatibility, say 'not_luadoc=true'
-- @module four
-- @author bob, james
-- @license MIT
-- @copyright InfoReich 2013

--- a function with typed args.
-- Note the the standard tparam aliases, and how the 'opt' and 'optchain'
-- modifiers may also be used. If the Lua function has varargs, then
-- you may document an indefinite number of extra arguments!
-- @tparam ?string|Person name person's name
-- @int age
-- @string[opt='gregorian'] calender optional calendar
-- @int[opt=0] offset optional offset
-- @treturn string
-- @see file:write
function one (name,age,...)
end

---- testing [opt]
-- @param one
-- @param[opt] two
-- @param three
-- @param[opt] four
function two (one,two,three,four)
end

--- third useless function.
-- Can always put comments inline, may
-- be multiple.
-- note that first comment refers to return type!
function three ( -- person:
    name, -- string: person's name
    age  -- int:
        -- not less than zero!
)
end

---- function with single optional arg
-- @param[opt] one
function four (one)
end

--- an implicit table.
-- Again, we can use the comments
person = {
    name = '', -- string: name of person
    age = 0, -- int:
}

--- an explicit table.
-- Can now use tparam aliases in table defns
-- @string name
-- @int age
-- @table person2


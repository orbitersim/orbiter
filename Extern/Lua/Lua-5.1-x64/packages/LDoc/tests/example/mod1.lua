---------------------------
-- Test module providing bonzo.dog.
-- Rest is a longer description
-- @class module
-- @name mod1

--- zero function. Two new ldoc features here; item types
-- can be used directly as tags, and aliases for tags
-- can be defined in config.lp.
-- @function zero_fun
-- @p k1 first
-- @p k2 second

--- first function. Some description
-- @param p1 first parameter
-- @param p2 second parameter
function mod1.first_fun(p1,p2)
end

-------------------------
-- second function.
-- @param ... var args!
function mod1.second_function(...)
end

------------
-- third function. Can also provide parameter comments inline,
-- provided they follow this pattern.
function mod1.third_function(
    alpha, -- correction A
    beta, -- correction B
    gamma -- factor C
    )
end

-----
-- A useful macro. This is an example of a custom 'kind'.
-- @macro first_macro
-- @see second_function

---- general configuration table
-- @table config
-- @field A alpha
-- @field B beta
-- @field C gamma
mod1.config = {
    A = 1,
    B = 2,
    C = 3
}

--[[--
Another function. Using a Lua block comment
@param p a parameter
]]
function mod1.zero_function(p)
end


-------
-- Multiple params may match a varargs function.
-- Generally, ldoc tries to be strict about matching params and formal arguments,
-- but this is relaxed for varargs: `function other(p,...)`
-- @param p
-- @param q
-- @param r
function mod1.other(p,...)
-- something cunning with select(2,...)
end

-------
-- A function with typed arguments.
-- The tparam tag is followed by the 'type'. There is no standard way
-- to represent Lua types, but you can adopt a convention. Type names
-- will be resolved. treturn must include a description after the type.
-- @tparam string name
-- @tparam number age
-- @treturn string modified age
function mod1.typed(name,age)

end

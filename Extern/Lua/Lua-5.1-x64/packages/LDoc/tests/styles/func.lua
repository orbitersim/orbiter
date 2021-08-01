------------
-- Get length of string.
-- A (silly) module which returns a single function
--
-- @module func
-- @string some text
-- @param opts multibyte encoding options
-- @string opts.charset encoding used
-- @bool opts.strict be very pedantic
-- @bool verbose tell the world about everything
-- @return its length
return function(s,opts,verbose)
    return #s
end


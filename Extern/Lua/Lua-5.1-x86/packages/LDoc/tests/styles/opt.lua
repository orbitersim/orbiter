------------
-- ### Functions with options and custom tags.
-- (use `ldoc -c opt.ld opt.lua` for converting.)
--
-- @include opt.md

---- testing [opt]
-- @param one
-- @param[opt] two
-- @param[opt] three
-- @param[opt] four
-- @remark use with caution!
function use_opt (one,two,three,four)
end

--- an explicit table.
-- Can use tparam aliases in table defns
-- @string name
-- @int[opt=0] age
-- @table person2


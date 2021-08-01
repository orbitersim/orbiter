----------------
-- Testing annotations
-- @module annot

--- first fun.
function first()
   if boo then
      local bar = do_something()
      if bar then
      ---  @fixme otherwise do what?
      end
   end
end

--- second try.
function second()
   --- @todo also handle foo case
   if bar then

   end
end

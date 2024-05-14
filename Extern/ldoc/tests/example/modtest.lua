-------
-- A script.
-- Scripts are not containers in the sense that modules are,
-- (although perhaps the idea of 'commands' could be adopted for some utilities)
-- It allows any upfront script comments to be included in the
-- documentation. Any long string marked with the 'usage' tag will also appear
-- in this area.
--
-- @script modtest

--- @usage
local usage = [[
modtest NAME
where NAME is your favourite name!

]]

print ('hello',arg[1])

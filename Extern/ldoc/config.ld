project = "LDoc"
title = "LDoc documentation"
description = "A Lua documentation tool"
format = "discount"
backtick_references = false
file = "ldoc.lua"
dir = "public"
readme = "manual.md"
style = "!new"
kind_names = {
   topic = "Manual",
   script = "Programs"
}
examples  =  {
   "tests/styles/colon.lua",
   "tests/styles/four.lua",
   "tests/styles/three.lua",
   "tests/styles/multiple.lua",
   "tests/example/mylib.c",
   "tests/moonscript/List.moon",
}
-- vim: ft=lua

unused_args     = false
redefined       = false
max_line_length = false

files = {
   ["config.ld"] = {
      std = "+ldoc"
   }
}

include_files = {
   "config.ld",
   "**/*.lua",
   "*.rockspec",
   ".luacheckrc",
}

exclude_files = {
   -- Tests are too messy to lint
   "tests",
   -- Travis Lua environment
   "here/*.lua",
   "here/**/*.lua",
   -- GH Actions Lua Environment
   ".lua",
   ".luarocks",
   ".install",
}

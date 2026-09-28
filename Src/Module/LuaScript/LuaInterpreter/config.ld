project='Orbiter'
title='Orbiter Lua Script interface'
description='Orbiter interface for Lua scripts'
format='markdown'
backtick_references=false
readme='intro.md'
wrap=true
-- not upstream: Interpreter.cpp in the case it has on disk (Linux file names are case sensitive)
file={'./Interpreter.cpp','./lua_vessel_mtd.cpp','./lua_xrsound.cpp','./types.lua'}
dir='out'
style='!fixed'
kind_names={topic='Manual',script='Programs'}
examples = {
}


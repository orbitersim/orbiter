maxline = 20
nline = 0
first_line = 0
linebuf = {}

function disp_output()
end

function add_line(line)
	oapi.dbg_out(line)
	oapi.write_log(line)
end

function assert(cond)
	if cond == false then
		add_line(" - FAILED!")
		error("Assertion failed\n"..debug.traceback())
        oapi.exit(1)
	end
end

function pass()
	add_line(" - passed")
end


-- ---------------------------------------------------
-- "Constants"
-- ---------------------------------------------------

data = "Hello world! Hello world! Hello world! Hello world!\n"
    .. "Hello world! Hello world! Hello world! Hello world!\n"
    .. "Hello world! Hello world! Hello world! Hello world!"
vec = { x = 1.2, y = -3.4, z = 5.6 }


-- ---------------------------------------------------
-- TEST(S)
-- ---------------------------------------------------

add_line("=== Lua script unit tests ===")
add_line("")

add_line("--- oapi module ---")

-- ---------------------------------------------------
add_line("Test: oapi.get_orbiter_version()")
-- ---------------------------------------------------
value = oapi.get_orbiter_version()
assert( value ~= nil )
assert( type(value) == "number" )
add_line("   Version " .. tostring(value))
pass()
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.rand()")
-- ---------------------------------------------------
value = oapi.rand()
assert( value ~= nil )
assert( type(value) == "number" )
assert( (value <= 1.0) and (value >= 0.0) )
assert( value ~= oapi.rand() ) -- very unlikely ;)
pass()
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.deflate()")
-- ---------------------------------------------------
zdata = oapi.deflate(data) -- zipped data
assert( zdata ~= nil   )
assert( #zdata < #data ) -- 30 < 155
assert( #zdata == 30   )
pass()
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.inflate()")
-- ---------------------------------------------------
udata = oapi.inflate(zdata) -- unzipped zdata
assert( udata ~= nil    )
assert( #udata == #data ) -- both 155 in size
assert( udata == data   ) -- should be equal
pass()
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.formatvalue(value,prec)")
-- ---------------------------------------------------
-- some precisions...
assert( oapi.formatvalue(PI,2)  == " 3")
assert( oapi.formatvalue(PI,3)  == " 3.1")
assert( oapi.formatvalue(PI,4)  == " 3.14")
assert( oapi.formatvalue(PI,5)  == " 3.142")
assert( oapi.formatvalue(PI,6)  == " 3.1416")
assert( oapi.formatvalue(PI,7)  == " 3.14159")
assert( oapi.formatvalue(PI,8)  == " 3.141593")
assert( oapi.formatvalue(PI,9)  == " 3.1415927")
assert( oapi.formatvalue(PI,10) == " 3.14159265")
assert( oapi.formatvalue(PI,11) == " 3.141592654")
-- regular postfixes
assert( oapi.formatvalue(PI*1e1 ) == " 31.42"  )
assert( oapi.formatvalue(PI*1e2 ) == " 314.2"  )
assert( oapi.formatvalue(PI*1e3 ) == " 3.142k" )
assert( oapi.formatvalue(PI*1e4 ) == " 31.42k" )
assert( oapi.formatvalue(PI*1e5 ) == " 314.2k" )
assert( oapi.formatvalue(PI*1e6 ) == " 3.142M" )
assert( oapi.formatvalue(PI*1e7 ) == " 31.42M" )
assert( oapi.formatvalue(PI*1e8 ) == " 314.2M" )
assert( oapi.formatvalue(PI*1e9 ) == " 3.142G" )
assert( oapi.formatvalue(PI*1e10) == " 31.42G" )
assert( oapi.formatvalue(PI*1e11) == " 314.2G" )
assert( oapi.formatvalue(PI*1e12) == " 3.142T" )
assert( oapi.formatvalue(PI*1e13) == " 31.42T" )
assert( oapi.formatvalue(PI*1e14) == " 314.2T" )
assert( oapi.formatvalue(PI*1e15) == " 3e+15"  )
assert( oapi.formatvalue(PI*1e16) == " 3e+16"  )
assert( oapi.formatvalue(PI*1e17) == " 3e+17"  )
assert( oapi.formatvalue(PI*1e18) == " 3e+18"  )
assert( oapi.formatvalue(PI*1e19) == " 3e+19"  )
assert( oapi.formatvalue(PI*1e20) == " 3e+20"  )
-- negative values
assert( oapi.formatvalue(-PI*1e1 ) == "-31.42"  )
assert( oapi.formatvalue(-PI*1e2 ) == "-314.2"  )
assert( oapi.formatvalue(-PI*1e3 ) == "-3.142k" )
assert( oapi.formatvalue(-PI*1e4 ) == "-31.42k" )
assert( oapi.formatvalue(-PI*1e5 ) == "-314.2k" )
assert( oapi.formatvalue(-PI*1e6 ) == "-3.142M" )
assert( oapi.formatvalue(-PI*1e7 ) == "-31.42M" )
assert( oapi.formatvalue(-PI*1e8 ) == "-314.2M" )
assert( oapi.formatvalue(-PI*1e9 ) == "-3.142G" )
assert( oapi.formatvalue(-PI*1e10) == "-31.42G" )
assert( oapi.formatvalue(-PI*1e11) == "-314.2G" )
assert( oapi.formatvalue(-PI*1e12) == "-3.142T" )
assert( oapi.formatvalue(-PI*1e13) == "-31.42T" )
assert( oapi.formatvalue(-PI*1e14) == "-314.2T" )
assert( oapi.formatvalue(-PI*1e15) == "-3e+15"  )
assert( oapi.formatvalue(-PI*1e16) == "-3e+16"  )
assert( oapi.formatvalue(-PI*1e17) == "-3e+17"  )
assert( oapi.formatvalue(-PI*1e18) == "-3e+18"  )
assert( oapi.formatvalue(-PI*1e19) == "-3e+19"  )
assert( oapi.formatvalue(-PI*1e20) == "-3e+20"  )
pass()
-- ---------------------------------------------------



-- ---------------------------------------------------
-- FINAL RESULT
-- ---------------------------------------------------

add_line("=== All tests passed ===")
oapi.exit(0)

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

fname_root = "__delete_me__.txt"


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
-- rediculous precisions...
assert( oapi.formatvalue(PI,0)  == " 3.141593")
assert( oapi.formatvalue(PI,1)  == " 3.141593")
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
assert( oapi.formatvalue(PI,12) == " 3.1415926536")
assert( oapi.formatvalue(PI,13) == " 3.14159265359")
assert( oapi.formatvalue(PI,14) == " 3.141592653590")
assert( oapi.formatvalue(PI,15) == " 3.1415926535898")
assert( oapi.formatvalue(PI,16) == " 3.14159265358979")
assert( oapi.formatvalue(PI,17) == " 3.141592653589790")
assert( oapi.formatvalue(PI,18) == " 3.1415926535897900")
assert( oapi.formatvalue(PI,19) == " 3.14159265358979001")
assert( oapi.formatvalue(PI,20) == " 3.141592653589790007")
assert( oapi.formatvalue(PI,21) == " 3.1415926535897900074")
assert( oapi.formatvalue(PI,22) == " 3.14159265358979000737")
assert( oapi.formatvalue(PI,23) == " 3.141592653589790007373")
assert( oapi.formatvalue(PI,24) == " 3.1415926535897900073735")
assert( oapi.formatvalue(PI,25) == " 3.14159265358979000737349")
assert( oapi.formatvalue(PI,26) == " 3.141592653589790007373495")
assert( oapi.formatvalue(PI,27) == " 3.1415926535897900073734945")
assert( oapi.formatvalue(PI,28) == " 3.14159265358979000737349452")
assert( oapi.formatvalue(PI,29) == " 3.141592653589790007373494518")
assert( oapi.formatvalue(PI,30) == " 3.1415926535897900073734945181")
assert( oapi.formatvalue(PI,31) == " 3.14159265358979000737349451811")
assert( oapi.formatvalue(PI,32) == " 3.141592653589790007373494518106")
assert( oapi.formatvalue(PI,33) == " 3.1415926535897900073734945181059")
assert( oapi.formatvalue(PI,34) == " 3.14159265358979000737349451810587")
assert( oapi.formatvalue(PI,35) == " 3.141592653589790007373494518105872")
assert( oapi.formatvalue(PI,36) == " 3.1415926535897900073734945181058720")
assert( oapi.formatvalue(PI,37) == " 3.14159265358979000737349451810587198")
assert( oapi.formatvalue(PI,38) == " 3.141592653589790007373494518105871975")
assert( oapi.formatvalue(PI,39) == " 3.1415926535897900073734945181058719754")
assert( oapi.formatvalue(PI,40) == " 3.14159265358979000737349451810587197542")
assert( oapi.formatvalue(PI,41) == " 3.141592653589790007373494518105871975422")
assert( oapi.formatvalue(PI,42) == " 3.1415926535897900073734945181058719754219")
assert( oapi.formatvalue(PI,43) == " 3.14159265358979000737349451810587197542191")
assert( oapi.formatvalue(PI,44) == " 3.141592653589790007373494518105871975421906")
assert( oapi.formatvalue(PI,45) == " 3.1415926535897900073734945181058719754219055")
assert( oapi.formatvalue(PI,46) == " 3.14159265358979000737349451810587197542190552")
assert( oapi.formatvalue(PI,47) == " 3.141592653589790007373494518105871975421905518")
assert( oapi.formatvalue(PI,48) == " 3.1415926535897900073734945181058719754219055176")
assert( oapi.formatvalue(PI,49) == " 3.14159265358979000737349451810587197542190551758")
assert( oapi.formatvalue(PI,50) == " 3.141592653589790007373494518105871975421905517578")
assert( oapi.formatvalue(PI,51) == " 3.1415926535897900073734945181058719754219055175781")
assert( oapi.formatvalue(PI,52) == " 3.14159265358979000737349451810587197542190551757812")
assert( oapi.formatvalue(PI,53) == " 3.141592653589790007373494518105871975421905517578125")
assert( oapi.formatvalue(PI,54) == " 3.1415926535897900073734945181058719754219055175781250")
assert( oapi.formatvalue(PI,55) == " 3.14159265358979000737349451810587197542190551757812500")
assert( oapi.formatvalue(PI,56) == " 3.141592653589790007373494518105871975421905517578125000")
assert( oapi.formatvalue(PI,57) == " 3.1415926535897900073734945181058719754219055175781250000")
assert( oapi.formatvalue(PI,58) == " 3.14159265358979000737349451810587197542190551757812500000")
assert( oapi.formatvalue(PI,59) == " 3.141592653589790007373494518105871975421905517578125000000")
assert( oapi.formatvalue(PI,60) == " 3.1415926535897900073734945181058719754219055175781250000000")
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

--[[

-- ---------------------------------------------------
add_line("Test: oapi.openfile(fname,FILE_OUT,root)")
-- ---------------------------------------------------
-- We need to create this file in ROOT first,
--   so we can be sure it is present when we do the
--   read-tests later on
mode = FILE_ACCESS_MODE.FILE_OUT
add_line("   ...FILE_OUT write (overwrite)")
f = oapi.openfile(fname_root, mode)
assert(f ~= nil)
oapi.closefile(f, mode)
pass()
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.openfile(fname,FILE_IN,...)")
-- ---------------------------------------------------
mode = FILE_ACCESS_MODE.FILE_IN
add_line("   ...ROOT Orbiter main directory")
f = oapi.openfile(fname_root, mode, PATH_ROOT.ROOT)
assert(f ~= nil)
oapi.closefile(f, mode)
pass()
-- ---------------------------------------------------

--]]


-- ---------------------------------------------------
-- FINAL RESULT
-- ---------------------------------------------------

add_line("=== All tests passed ===")
oapi.exit(0)

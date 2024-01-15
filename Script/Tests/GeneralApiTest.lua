-- ---------------------------------------------------
-- General TEST Utils
-- ---------------------------------------------------

local tests_passed = 0 -- to be filled with TEST_ID.xxx

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

function pass(test_id)
	add_line(" - passed")
	tests_passed = tests_passed + test_id
end

-- ---------------------------------------------------
-- "Constants"
-- ---------------------------------------------------

local TEST_ID = {
  openfile_read  = 0x001,
  openfile_write = 0x002,
  writeline      = 0x004,
  writeitem_xxx  = 0x008,
  readitem_xxx   = 0x010,
  rand           = 0x020,
  deflate        = 0x040,
  inflate        = 0x080,
  formatvalue    = 0x100
}
local ALL_PASSED = 0x1FF -- make sure this is the sum of TEST_ID values!

local data = "Hello world! Hello world! Hello world! Hello world!\n"
          .. "Hello world! Hello world! Hello world! Hello world!\n"
          .. "Hello world! Hello world! Hello world! Hello world!"
local vec = {x=1.2, y=-3.4, z=5.6}


-- ---------------------------------------------------
-- TEST(S)
-- ---------------------------------------------------

add_line("=== Lua script unit tests ===")
add_line("")

add_line("--- oapi module ---")

-- ---------------------------------------------------
add_line("Test: oapi.openfile(fname,mode,root) read")
-- ---------------------------------------------------
mode = FILE_ACCESS_MODE.FILE_IN
add_line("   ...ROOT Orbiter main directory")
f = oapi.openfile("keymap.cfg", mode, PATH_ROOT.ROOT)
assert(f ~= nil) ; oapi.closefile(f, mode)

add_line("   ...CONFIG Orbiter config folder")
f = oapi.openfile("Sun.cfg", mode, PATH_ROOT.CONFIG)
assert(f ~= nil) ; oapi.closefile(f, mode)

add_line("   ...SCENARIOS Orbiter scenarios folder")
f = oapi.openfile("Demo\\Description.txt", mode, PATH_ROOT.SCENARIOS)
assert(f ~= nil) ; oapi.closefile(f, mode)

add_line("   ...TEXTURES Orbiter standard texture folder")
f = oapi.openfile("transp.dds", mode, PATH_ROOT.TEXTURES)
assert(f ~= nil) ; oapi.closefile(f, mode)

add_line("   ...TEXTURES2 Orbiter high-res texture folder")
f = oapi.openfile("DG\\dgmk4_1.dds", mode, PATH_ROOT.TEXTURES2)
assert(f ~= nil) ; oapi.closefile(f, mode)

add_line("   ...MESHES Orbiter mesh folder")
f = oapi.openfile("dummy.msh", mode, PATH_ROOT.MESHES)
assert(f ~= nil) ; oapi.closefile(f, mode)

add_line("   ...MODULES Orbiter module folder")
f = oapi.openfile("ScriptVessel.dll", mode, PATH_ROOT.MODULES)
assert(f ~= nil) ; oapi.closefile(f, mode)

add_line("   ...FILE_IN vs. FILE_IN_ZEROONFAIL on non-existing file")
f1 = oapi.openfile("nofile.txt", FILE_ACCESS_MODE.FILE_IN)
f2 = oapi.openfile("nofile.txt", FILE_ACCESS_MODE.FILE_IN_ZEROONFAIL)
assert(f1 ~= f2)
assert(f1 ~= nil) ; oapi.closefile(f1, FILE_ACCESS_MODE.FILE_IN)
assert(f2 == nil) ; oapi.closefile(f2, FILE_ACCESS_MODE.FILE_IN_ZEROONFAIL)
pass(TEST_ID.openfile_read)
-- ---------------------------------------------------

-- ---------------------------------------------------
add_line("Test: oapi.openfile(fname,mode,root) write")
-- ---------------------------------------------------
fname = "__delete_me__.txt"
local function cleanup() -- to be called at end of test
	os.remove(fname) -- does the path fit?
end

add_line("   ...FILE_OUT write (overwrite)")
f = oapi.openfile(fname, FILE_ACCESS_MODE.FILE_OUT)
assert(f ~= nil)
oapi.closefile(f, FILE_ACCESS_MODE.FILE_OUT)

add_line("   ...FILE_APP write (append)")
f = oapi.openfile(fname, FILE_ACCESS_MODE.FILE_APP)
assert(f ~= nil)
-- oapi.closefile(f, FILE_ACCESS_MODE.FILE_APP) -- NOT yet! oapi_writeitem_xxx test use it!
pass(TEST_ID.openfile_write)
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.writeline(f,line)")
-- ---------------------------------------------------
oapi.writeline(f, "# >>> This is a test-artifact and can be deleted! <<<");
oapi.writeline(f, "");
pass(TEST_ID.writeline)
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.writeitem_xxx(f,item,value)")
-- ---------------------------------------------------
add_line("   ...oapi.writeitem_string()")
oapi.writeitem_string(f, "VAL_STR", "foo")
add_line("   ...oapi.writeitem_float()")
oapi.writeitem_float(f, "VAL_FLOAT", PI)
add_line("   ...oapi.writeitem_int()")
oapi.writeitem_int(f, "VAL_INT", 4711)
add_line("   ...oapi.writeitem_bool()")
oapi.writeitem_bool(f, "VAL_BOOL[0]", false)
oapi.writeitem_bool(f, "VAL_BOOL[1]", true)
add_line("   ...oapi.writeitem_vec()")
oapi.writeitem_vec(f, "VAL_VEC", vec)
oapi.closefile(f, FILE_ACCESS_MODE.FILE_APP)
pass(TEST_ID.writeitem_xxx)
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.readitem_xxx(f,item)")
-- ---------------------------------------------------
f = oapi.openfile(fname, FILE_ACCESS_MODE.FILE_IN)
assert(f ~= nil)
add_line("   ...oapi.readitem_vec()")
vec2 = oapi.readitem_vec(f, "VAL_VEC")
assert( vec2.x == vec.x and vec2.y == vec.y and vec2.z == vec.z )
add_line("   ...oapi.readitem_bool()")
assert( oapi.readitem_bool(f, "VAL_BOOL[1]") == true )
assert( oapi.readitem_bool(f, "VAL_BOOL[0]") == false )
add_line("   ...oapi.readitem_int()")
assert( oapi.readitem_int(f, "VAL_INT") == 4711 )
add_line("   ...oapi.readitem_float()")
assert( oapi.readitem_float(f, "VAL_FLOAT") == 3.14159 ) -- close enough ;)
add_line("   ...oapi.readitem_string()")
assert( oapi.readitem_string(f, "VAL_STR") == "foo" )
oapi.closefile(f, FILE_ACCESS_MODE.FILE_IN)
pass(TEST_ID.readitem_xxx)
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.rand()")
-- ---------------------------------------------------
assert( oapi.rand() <= 1.0 ) -- not much to "test" ;)
assert( oapi.rand() >= 0.0 )
assert( oapi.rand() ~= oapi.rand() ) -- very unlikely ;)
pass(TEST_ID.rand)
-- ---------------------------------------------------


-- ---------------------------------------------------
add_line("Test: oapi.deflate()")
-- ---------------------------------------------------
-- assert( #data == 155 )
zdata = oapi.deflate(data) -- zipped data
assert( #zdata < #data ) -- 30 < 155
assert( #zdata == 30 )
pass(TEST_ID.deflate)
-- ---------------------------------------------------

-- ---------------------------------------------------
add_line("Test: oapi.inflate()")
-- ---------------------------------------------------
udata = oapi.inflate(zdata) -- unzipped zdata
assert( #udata == #data ) -- both 155 in size
assert( udata == data ) -- should be equal
pass(TEST_ID.inflate)
-- ---------------------------------------------------


--[[ get_color doesn't seem to work in "headless" CI test :(
-- ---------------------------------------------------
add_line("Test: oapi.get_color(r,g,b)")
-- ---------------------------------------------------
assert( oapi.get_color(  0,  0,  0)  == 0        )
assert( oapi.get_color(  0,  0,255)  == 255      )
assert( oapi.get_color(  0,255,  0)  == 65280    )
assert( oapi.get_color(255,  0,  0)  == 16711680 )
assert( oapi.get_color(  0,255,255)  == 65535    )
assert( oapi.get_color(255,255,255)  == 16777215 )
assert( oapi.get_color(255,255,  0)  == 16776960 )
assert( oapi.get_color(  1,  2,  3)  == 66051    )
pass(TEST_ID.get_color)
-- ---------------------------------------------------
--]]


-- ---------------------------------------------------
add_line("Test: oapi.formatvalue(f,p)")
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
-- negative
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
pass(TEST_ID.formatvalue)
-- ---------------------------------------------------


-- ---------------------------------------------------
-- FINAL RESULT
-- ---------------------------------------------------
assert(tests_passed == ALL_PASSED)
add_line("=== All tests passed ===")
cleanup()
--proc.wait_simdt(5) -- just for GUI checking...
oapi.exit(0)
-- ---------------------------------------------------
-- General TEST Utils
-- ---------------------------------------------------

-- fifo_add --
local linebuf = {}
local linenum = 0
local viewstart = 0
local linemax = 18       -- view window size
-- periodic_check --
local pc_inhibit = false -- flag to inhibit periodic_check()
local tmax = 5           -- 5 seconds should be plenty of time to complete this test
local t0 = oapi.get_systime()
-- general --
local tests_passed = 0   -- to be filled with TEST_ID.xxx

function fifo_add(line)
	linebuf[linenum] = line
	linenum = linenum + 1
	if linenum > linemax then
		viewstart = viewstart + 1
	end
end

function add_line(line)
	oapi.dbg_out(line)
	oapi.write_log(line)
	fifo_add(line)
end

function assert(cond)
	if cond == false then
		add_line(" - FAILED!")
		error("Assertion failed\n"..debug.traceback())
		pc_inhibit = true -- inhibit periodic_check. no more checks!
        -- oapi.exit(1)
	end
end

function pass(test_id)
	add_line(" - passed")
	tests_passed = tests_passed + test_id
end

-- local pc_count = 0
function periodic_check()
	if pc_inhibit then return end

	local t1 = oapi.get_systime()
	if t1 - t0 > tmax then -- timeout => check
		-- add_line("timeout")
		-- assert( tests_passed == ALL_PASSED )
		if tests_passed == ALL_PASSED then
			add_line("=== All tests passed ===")
		else
			add_line("=== Some tests failed to pass (in time) ===")
			-- oapi.exit(1)
		end
		pc_inhibit = true -- inhibit periodic_check. no more checks!
		cleanup()
		-- oapi.exit(0)
	end
	-- All tests passed before timeout?
	if tests_passed == ALL_PASSED then
		add_line("=== All tests passed ===")
		pc_inhibit = true -- inhibit periodic_check. no more checks!
		-- oapi.exit(0)
		cleanup()
	end
	-- <DEBUG>
	-- pc_count = pc_count + 1
	-- linebuf[linenum-1] = tostring(pc_count) .. " t1:" .. oapi.formatvalue(t1) .. " t0:" .. oapi.formatvalue(t0)
	-- </DEBUG>
end

function cleanup()
	res,err = os.remove("Scenarios\\Tests\\Manual\\__delete_me__.scn")
	if err ~= nil then
		add_line(err)
	end
end

-- ---------------------------------------------------
-- "Constants"
-- ---------------------------------------------------

local TEST_ID = {
  valid_mfd_table     = 0x01,
  readstatus_called   = 0x02,
  recallstatus_called = 0x04,
  writestatus_called  = 0x08,
  update_called       = 0x10,
  savescenario        = 0x20
}
local ALL_PASSED      = 0x3F -- make sure this is the sum of TEST_ID values!

vec = { x = 1.2, y = -3.4, z = 5.6 }


-- ---------------------------------------------------
-- TEST(S)
-- ---------------------------------------------------

add_line("=== Lua script unit tests ===")

add_line("")
add_line("--- Script MFD module ---")


-- ---------------------------------------------------
-- MFD ''globals'
-- ---------------------------------------------------
add_line("check vor valid 'mfd' table")
assert(mfd ~= nil)
pass(TEST_ID.valid_mfd_table)


-- ---------------------------------------------------
--  readstatus, oapi.readscenario_nextline
-- ---------------------------------------------------
function readstatus(scn)
	add_line("readstatus(scn) called")
	assert(scn ~= nil)

	local got_str = false
	local got_int = false
	local got_flt = false
	local got_vec = false
	while true do
		line = oapi.readscenario_nextline(scn)
		if line == nil then -- ALTERNATIVE(see "else", too): if line == nil or line == "END_MFD" then
			break
		elseif string.find(line, "VAL_STR") then
			got_str = true -- VAL_STR foo
			add_line("  | got \"" .. line .. "\"")
		elseif string.find(line, "VAL_INT") then
			got_int = true -- VAL_INT 42
			add_line("  | got \"" .. line .. "\"")
		elseif string.find(line, "VAL_FLT") then
			got_flt = true -- VAL_FLT 3.14
			add_line("  | got \"" .. line .. "\"")
		elseif string.find(line, "VAL_VEC") then
			got_vec = true -- VAL_VEC 1.20 -3.40 5.60
			add_line("  | got \"" .. line .. "\"")
		else                  -- "END_MFD" etc. ...why??? C++ API does this too!
			add_line("  | got unexpected \"" .. line .. "\"")
		end
	end
	assert(got_str == true and got_int == true and got_flt == true and got_vec == true)
	pass(TEST_ID.readstatus_called)
end

-- ---------------------------------------------------
--  recallstatus (TO BE DONE!)
-- ---------------------------------------------------
function recallstatus()
	add_line("recallstatus() called")
	pass(TEST_ID.recallstatus_called)
end

-- ---------------------------------------------------
--  writestatus, oapi.writescenario_XXX
-- ---------------------------------------------------
function writestatus(scn)
	add_line("writestatus(scn) called")
	assert(scn ~= nil)
	oapi.writescenario_string(scn, "VAL_STR", "foo")
	oapi.writescenario_int   (scn, "VAL_INT", 42)
	oapi.writescenario_float (scn, "VAL_FLT", 3.141592)
	-- it's no problem writing same keys over and over...
	oapi.writescenario_float (scn, "VAL_FLT", 0.0)
	oapi.writescenario_float (scn, "VAL_FLT", 0.1)
	oapi.writescenario_float (scn, "VAL_FLT", 1.0)
	oapi.writescenario_float (scn, "VAL_FLT", 4  ) -- will both become "VAL_FLT 4.0"
	oapi.writescenario_float (scn, "VAL_FLT", 4.0) --  "    "      "        "
	oapi.writescenario_vec   (scn, "VAL_VEC", vec)
	pass(TEST_ID.writestatus_called)
end

-- ---------------------------------------------------
--  prestep, poststep (TO BE DONE!)
-- ---------------------------------------------------
function prestep(simt,simdt,mjd)
end

function poststep(simt,simdt,mjd)
end

-- ---------------------------------------------------
--  update
-- ---------------------------------------------------
local update_called_once = false
function update(skp)
	-- For the Test result ...
	if update_called_once == false then
		update_called_once = true
		add_line("update(skp) called")
		assert(skp ~= nil)
		add_line("valid 'skp'")
		pass(TEST_ID.update_called)

		add_line("call oapi.savescenario(fname,desc)")
		oapi.savescenario( "Tests\\Manual\\__delete_me__",
		                   "This is a test-artifact and can be deleted!" )
		pass(TEST_ID.savescenario)
	end
	periodic_check() -- ... for "ALL TESTS PASSED" condition

	-- For eye candy ;) ...
	ch,cw = skp:get_charsize()
    mfd:set_title(skp,'Script MFD Test')
	top = 2 * ch
	lft = 4 * cw
	for i=0,linemax do
		line = linebuf[viewstart+i-1]
		skp:text(lft, top + i * ch, line, #line)
	end
	return true
end




-- ---------------------------------------------------
-- FINAL RESULT
-- ---------------------------------------------------
-- look @ update() & periodic_check()

-- add_line("=== All tests passed ===")
-- oapi.exit(0)
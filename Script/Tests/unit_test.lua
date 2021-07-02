note = oapi.create_annotation()
note:set_pos (0.2,0.1,0.8,0.9);
note:set_size(0.5)
note:set_colour ({r=0.7,g=0.8,b=1})

maxline = 20
nline = 0
first_line = 0
linebuf = {}

function disp_output()
	buf = ""
	for i=0,nline-1 do
		idx = first_line + i
		if idx >= maxline then
			idx = idx - maxline
		end
		buf = buf .. linebuf[idx] .. "\n"
	end
	note:set_text(buf)
end

function add_line(line)
	idx = first_line + nline
	if idx >= maxline then
		idx = idx - maxline
	end
	linebuf[idx] = line
	if nline < maxline then
		nline = nline + 1
	else
		first_line = first_line + 1
		if first_line >= maxline then
			first_line = first_line - maxline
		end
	end
	disp_output()
end

function assert(cond)
	if cond == false then
		add_line("Test failed!")
		error("Test failed!")
	end
end

function pass()
	idx = first_line + nline - 1
	if idx >= maxline then
		idx = idx - maxline
	end
	linebuf[idx] = linebuf[idx] .. " - passed!"
	disp_output()
	proc.wait_sysdt(0.5)
end

add_line("=== Lua script unit tests ===")

add_line("")
add_line("--- vessel module ---")

add_line("Test: vessel.get_focusinterface()")
v = vessel.get_focusinterface()
assert(v ~= nil)
assert(v:get_name() == "GL-01")
pass()

add_line("Test: vessel.get_interface(name)")
v = vessel.get_interface("GL-01")
assert(v ~= nil)
assert(v:get_name() == "GL-01")
v = vessel.get_interface("SH-03")
assert(v ~= nil)
assert(v:get_name() == "SH-03")
v = vessel.get_interface("dummy")
assert(v == nil)
pass()

add_line("Test: vessel.get_interface(idx)")
v = vessel.get_interface(0)
assert(v ~= nil)
assert(v:get_name() == "ISS")
v = vessel.get_interface(5)
assert(v ~= nil)
assert(v:get_name() == "PB-01")
v = vessel.get_interface(1000)
assert(v == nil)
v = vessel.get_interface(-1)
assert(v == nil)
pass()

add_line("Test: vessel.get_focushandle()")
h = vessel.get_focushandle()
assert(h ~= nil)
assert(oapi.get_objname(h) == "GL-01")
pass()

add_line("Test: vessel.get_handle(name)")
h = vessel.get_handle("GL-01")
assert(h ~= nil)
assert(oapi.get_objname(h) == "GL-01")
h = vessel.get_handle("SH-03")
assert(h ~= nil)
assert(oapi.get_objname(h) == "SH-03")
h = vessel.get_handle("dummy")
assert(h == nil)
pass()

add_line("Test: vessel.get_handle(idx)")
h = vessel.get_handle(0)
assert(h ~= nil)
assert(oapi.get_objname(h) == "ISS")
h = vessel.get_handle(5)
assert(h ~= nil)
assert(oapi.get_objname(h) == "PB-01")
h = vessel.get_handle(1000)
assert(h == nil)
h = vessel.get_handle(-1)
assert(h == nil)
pass()

add_line("Test: vessel.get_interface(handle)")
h = vessel.get_handle(0)
v = vessel.get_interface(h)
assert(v ~= nil)
assert(v:get_name() == "ISS")
h = vessel.get_handle(5)
v = vessel.get_interface(h)
assert(v ~= nil)
assert(v:get_name() == "PB-01")
v = vessel.get_interface(nil)
assert(v == nil)
pass()

add_line("Test: vessel.get_count()")
n = vessel.get_count()
assert(n == 8)
pass()

add_line("")
add_line("--- vessel class ---")

add_line("Test: vessel:get_handle()")
h = vessel.get_handle(0)
v = vessel.get_interface(h)
assert(v:get_handle() == h)
h = vessel.get_focushandle()
v = vessel.get_focusinterface()
assert(v:get_handle() == h)
pass()

add_line("Test: vessel:get_name()")
v = vessel.get_focusinterface()
assert(v:get_name() == "GL-01")
v = vessel.get_interface("ISS")
assert(v:get_name() == "ISS")
v = vessel.get_interface("PB-01")
assert(v:get_name() == "PB-01")
pass()

add_line("Test: vessel:get_classname()")
v = vessel.get_focusinterface()
assert(v:get_classname() == "DeltaGlider")
v = vessel.get_interface("ISS")
assert(v:get_classname() == "ProjectAlpha_ISS")
v = vessel.get_interface("PB-01")
assert(v:get_classname() == "ShuttlePB")
pass()

add_line("Test: vessel:get_enablefocus()")
v = vessel.get_focusinterface()
assert(v:get_enablefocus() == true)
v = vessel.get_interface("Luna-OB1")
assert(v:get_enablefocus() == false)
pass()

add_line("Test: vessel:set_enablefocus()")
v = vessel.get_interface("Luna-OB1")
v:set_enablefocus(true)
assert(v:get_enablefocus() == true)
v:set_enablefocus(false)
assert(v:get_enablefocus() == false)
v = vessel.get_interface("GL-02")
v:set_enablefocus(false)
assert(v:get_enablefocus() == false)
v:set_enablefocus(true)
assert(v:get_enablefocus() == true)
pass()

add_line("Test: vessel:get_size()")
v = vessel.get_interface("GL-01")
assert(v:get_size() == 10)
v = vessel.get_interface("ISS")
assert(v:get_size() == 55)
pass()

add_line("Test: vessel:set_size(number)")
v = vessel.get_interface("GL-01")
v:set_size(20)
assert(v:get_size() == 20)
v:set_size(10)
assert(v:get_size() == 10)
pass()

add_line("Test: vessel:get_emptymass()")
v = vessel.get_interface("GL-01")
assert(v:get_emptymass() == 11000)
v = vessel.get_interface("ISS")
assert(v:get_emptymass() == 450000)
pass()

add_line("Test: vessel:set_emptymass(number)")
v = vessel.get_interface("GL-01")
m = v:get_emptymass()
v:set_emptymass(1000)
assert(v:get_emptymass() == 1000)
v:set_emptymass(m)
pass()

add_line("Test: vessel:get_pmi()")
v = vessel.get_interface("GL-01")
pmi = v:get_pmi()
assert(pmi.x == 15.5 and pmi.y == 22.1 and pmi.z == 7.7)
v = vessel.get_interface("ISS")
pmi = v:get_pmi()
assert(pmi.x == 385 and pmi.y == 719 and pmi.z == 508)
pass()

add_line("Test: vessel:set_pmi(vector)")
v = vessel.get_interface("GL-01")
ppmi = v:get_pmi()
v:set_pmi({x=10, y=20, z=30})
pmi = v:get_pmi()
assert(pmi.x == 10 and pmi.y == 20 and pmi.z == 30)
v:set_pmi(ppmi)
pmi = v:get_pmi()
assert(pmi.x == 15.5 and pmi.y == 22.1 and pmi.z == 7.7)
pass()

add_line("Test: vessel:get_crosssections()")
v = vessel.get_interface("GL-01")
cs = v:get_crosssections()
assert(cs.x == 53 and cs.y == 186.9 and cs.z == 25.9)
v = vessel.get_interface("ISS")
cs = v:get_crosssections()
assert(cs.x == 634 and cs.y == 4103 and cs.z == 878)
pass()

add_line("Test: vessel:set_crosssections(vector)")
v = vessel.get_interface("GL-01")
pcs = v:get_crosssections()
v:set_crosssections({x=10, y=20, z=30})
cs = v:get_crosssections()
assert(cs.x == 10 and cs.y == 20 and cs.z == 30)
v:set_crosssections(pcs)
cs = v:get_crosssections()
assert(cs.x == 53 and cs.y == 186.9 and cs.z == 25.9)
pass()

add_line("Test: vessel:get_gravitygradientdamping()")
v = vessel.get_interface("GL-01")
assert(v:get_gravitygradientdamping() == 20)
v = vessel.get_interface("ISS")
assert(v:get_gravitygradientdamping() == 0)
pass()

add_line("Test: vessel:set_gravitygradientdamping(number)")
v = vessel.get_interface("GL-01")
pggd = v:get_gravitygradientdamping()
v:set_gravitygradientdamping(100)
assert(v:get_gravitygradientdamping() == 100)
v:set_gravitygradientdamping(pggd)
assert(v:get_gravitygradientdamping() == 20)
pass()

add_line("Test: vessel:get_touchdownpoints()")
v = vessel.get_interface("GL-01")
td1,td2,td3 = v:get_touchdownpoints()
assert(td1.x == 0 and td1.y == -1.5 and td1.z == 9 and td2.x == -6 and td2.y == -0.8 and td2.z == -5 and td3.x == 3 and td3.y == -1.2 and td3.z == -5)
v:Gear(1)
proc.wait_simdt(7)
td1,td2,td3 = v:get_touchdownpoints()
assert(td1.x == 0 and td1.y == -2.57 and td1.z == 10 and td2.x == -3.5 and td2.y == -2.57 and td2.z == -1 and td3.x == 3.5 and td3.y == -2.57 and td3.z == -1)
v:Gear(0)
proc.wait_simdt(7)
td1,td2,td3 = v:get_touchdownpoints()
assert(td1.x == 0 and td1.y == -1.5 and td1.z == 9 and td2.x == -6 and td2.y == -0.8 and td2.z == -5 and td3.x == 3 and td3.y == -1.2 and td3.z == -5)
pass()

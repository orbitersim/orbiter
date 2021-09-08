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

add_line("Test: vessel:version()")
v = vessel.get_interface("GL-01")
assert(v:version() == 3)
v = vessel.get_interface("PB-01")
assert(v:version() == 2)
v = vessel.get_interface("ISS")
assert(v:version() == 0)
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

-- add_line("Test: vessel:get_gravitygradientdamping()")
-- v = vessel.get_interface("GL-01")
-- assert(v:get_gravitygradientdamping() == 20)
-- v = vessel.get_interface("ISS")
-- assert(v:get_gravitygradientdamping() == 0)
-- pass()
-- 
-- add_line("Test: vessel:set_gravitygradientdamping(number)")
-- v = vessel.get_interface("GL-01")
-- pggd = v:get_gravitygradientdamping()
-- v:set_gravitygradientdamping(100)
-- assert(v:get_gravitygradientdamping() == 100)
-- v:set_gravitygradientdamping(pggd)
-- assert(v:get_gravitygradientdamping() == 20)
-- pass()

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

add_line("Test: vessel:get_touchdownpointcount()")
v = vessel.get_interface("GL-01")
tdc = v:get_touchdownpointcount()
assert(tdc == 13)
pass()

exp = {} -- expected "gear up" results
exp[ 0] = {pos={x=0, y=-1.5, z=9}, stiffness=1e7, damping=1e5, mu=3.5445e-08, mu_lng=3}
exp[ 1] = {pos={x=-6, y=-0.8, z=-5}, stiffness=1e7, damping=1e5, mu=2.21197e-08, mu_lng=3}
exp[ 2] = {pos={x=3, y=-1.2, z=-5}, stiffness=1e7, damping=1e5, mu=4.24352e-08, mu_lng=3}
exp[ 3] = {pos={x=-8.5, y=-0.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=-9.12894e+306, mu_lng=3}
exp[ 4] = {pos={x=8.5, y=-0.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=-1.63524e-206, mu_lng=3}
exp[ 5] = {pos={x=-8.5, y=-0.4, z=-3}, stiffness=1e7, damping=1e5, mu=2.00016, mu_lng=3}
exp[ 6] = {pos={x=8.5, y=-0.4, z=-3}, stiffness=1e7, damping=1e5, mu=1.03171e+26, mu_lng=3}
exp[ 7] = {pos={x=-8.85, y=2.3, z=-5.05}, stiffness=1e7, damping=1e5, mu=1.42505e+103, mu_lng=3}
exp[ 8] = {pos={x=8.85, y=2.3, z=-5.05}, stiffness=1e7, damping=1e5, mu=9.53414e-130, mu_lng=3}
exp[ 9] = {pos={x=-8.85, y=2.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=5.50352e-53, mu_lng=3}
exp[10] = {pos={x=8.85, y=2.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=5.88118e-308, mu_lng=3}
exp[11] = {pos={x=0,y=2, z=6.2}, stiffness=1e7, damping=1e5, mu=2.93186e-308, mu_lng=3}
exp[12] = {pos={x=0,y=-0.6, z=10.65}, stiffness=1e7, damping=1e5, mu=1.42005e-306, mu_lng=3}

exp2 = {} -- expected "gear down" results
exp2[0] = {pos={x=0, y=-2.57, z=10}, stiffness=1e6, damping=1e5, mu=9.09091e-08, mu_lng=1.6}
exp2[1] = {pos={x=-3.5, y=-2.57, z=-1}, stiffness=1e6, damping=1e5, mu=4.54545e-07, mu_lng=3}
exp2[2] = {pos={x=3.5, y=-2.57, z=-1}, stiffness=1e6, damping=1e5, mu=4.54545e-07, mu_lng=3}
exp2[3] = {pos={x=-8.5, y=-0.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=-5.10637e-310, mu_lng=3}
exp2[4] = {pos={x=8.5, y=-0.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=3.11088e-302, mu_lng=3}
exp2[5] = {pos={x=-8.5, y=-0.4, z=-3}, stiffness=1e7, damping=1e5, mu=0, mu_lng=3}
exp2[6] = {pos={x=8.5, y=-0.4, z=-3}, stiffness=1e7, damping=1e5, mu=0, mu_lng=3}
exp2[7] = {pos={x=-8.85, y=2.3, z=-5.05}, stiffness=1e7, damping=1e5, mu=0, mu_lng=3}
exp2[8] = {pos={x=8.85, y=2.3, z=-5.05}, stiffness=1e7, damping=1e5, mu=0, mu_lng=3}
exp2[9] = {pos={x=-8.85, y=2.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=nil, mu_lng=3}
exp2[10] = {pos={x=8.85, y=2.3, z=-7.05}, stiffness=1e7, damping=1e5, mu=-3.53744e-310, mu_lng=3}
exp2[11] = {pos={x=0, y=2, z=6.2}, stiffness=1e7, damping=1e5, mu=2.76101e-309, mu_lng=3}
exp2[12] = {pos={x=0, y=-0.6, z=10.65}, stiffness=1e7, damping=1e5, mu=0, mu_lng=3}

add_line("Test: vessel:get_touchdownpoints(GEARUP \"NewAPI\")")
v = vessel.get_interface("GL-01")
tdc = v:get_touchdownpointcount()
for i=0,tdc-1 do
  tdp = v:get_touchdownpoints(i)
  -- add_line("tdp [" .. i .. "] =" .. tostring(tdp))
  -- add_line("tdp [" .. i .. "] = {" .. tdp.pos.x .. ", " .. tdp.pos.y .. ", " .. tdp.pos.z .. "}")
  -- add_line("exp [" .. i .. "] = {" .. exp[i].pos.x .. ", " .. exp[i].pos.y .. ", " .. exp[i].pos.z .. "}")
  assert(tdp.pos.x == exp[i].pos.x and tdp.pos.y == exp[i].pos.y and tdp.pos.z == exp[i].pos.z
     and tdp.stiffness == exp[i].stiffness
     and tdp.damping == exp[i].damping
     -- and tdp.mu == exp[i].mu
     and tdp.mu_lng == exp[i].mu_lng
  )
end
pass()
v:Gear(1)
proc.wait_simdt(7)
add_line("Test: vessel:get_touchdownpoints(GEARDOWN \"NewAPI\")")
for i=0,tdc-1 do
  tdp = v:get_touchdownpoints(i)
  -- add_line("[" .. i .. "] = {pos=".. tdp.pos.x .. ", y=" .. tdp.pos.y .. ", z=" .. tdp.pos.z .. "},".."stiffness=".. tdp.stiffness .. ", damping=" .. tdp.damping .. "mu=nil, mu_lng=" .. tdp.mu_lng .. "}")
  assert(tdp.pos.x == exp2[i].pos.x and tdp.pos.y == exp2[i].pos.y and tdp.pos.z == exp2[i].pos.z
     and tdp.stiffness == exp2[i].stiffness
     and tdp.damping == exp2[i].damping
     -- and tdp.mu == exp2[i].mu
     and tdp.mu_lng == exp2[i].mu_lng
  )
end
pass()
v:Gear(0)
proc.wait_simdt(7)
add_line("Test: vessel:get_touchdownpoints(GEARUP2 \"NewAPI\")")
for i=0,tdc-1 do
  tdp = v:get_touchdownpoints(i)
  assert(tdp.pos.x == exp[i].pos.x and tdp.pos.y == exp[i].pos.y and tdp.pos.z == exp[i].pos.z
     and tdp.stiffness == exp[i].stiffness
     and tdp.damping == exp[i].damping
     -- and tdp.mu == exp[i].mu
     and tdp.mu_lng == exp[i].mu_lng
  )
end
pass()

add_line("Test: vessel:set_touchdownpoints(\"NewAPI\")")
v = vessel.get_interface("GL-01")
arr = {}
arr[1] = v:get_touchdownpoints(0) -- NEW, 'cause index provided
arr[2] = v:get_touchdownpoints(1) -- NEW,   "     "     "
arr[3] = v:get_touchdownpoints(2) -- NEW,   "     "     "
arr[4] = v:get_touchdownpoints(3) -- NEW,   "     "     "
arr[5] = v:get_touchdownpoints(4) -- NEW,   "     "     "
arr[6] = v:get_touchdownpoints(5) -- NEW,   "     "     "
v:set_touchdownpoints(arr)
pass()

add_line("=== All tests passed ===")
oapi.exit(0)

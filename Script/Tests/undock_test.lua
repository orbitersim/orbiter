-- ---------------------------------------------------------------------------
-- ANNOTATION HELPERS
-- ---------------------------------------------------------------------------
-- re-use 1st 'old' annotation object (if available)
note = oapi.get_annotations() or oapi.create_annotation()
note:set_pos (0.2,0.1,0.8,0.9);
note:set_size(0.5)
note:set_colour ({r=0.7,g=0.8,b=1})

maxline = 20
nline = 0
first_line = 0
linebuf = {}

function flush_output()
	local buf = ""
	for i=0,nline-1 do
		local idx = first_line + i
		if idx >= maxline then
			idx = idx - maxline
		end
		buf = buf .. linebuf[idx] .. "\n"
	end
	note:set_text(buf)
end

function clear_lines()
	for i=0,maxline do
		linebuf[i] = ""
	end
	nline = maxline
	flush_output()
end

function set_line(idx, line)
	if idx < maxline then
        linebuf[idx] = line
        if idx >= nline then
            nline = idx + 1
        end
        flush_output()
    end
end

-- function add_line(line)
-- 	local idx = first_line + nline
-- 	if idx >= maxline then
-- 		idx = idx - maxline
-- 	end
-- 	linebuf[idx] = line
-- 	if nline < maxline then
-- 		nline = nline + 1
-- 	else
-- 		first_line = first_line + 1
-- 		if first_line >= maxline then
-- 			first_line = first_line - maxline
-- 		end
-- 	end
-- 	flush_output()
-- end

-- function append_to_line(txt)
-- 	local idx = first_line + nline - 1
-- 	if idx >= maxline then
-- 		idx = idx - maxline
-- 	end
-- 	linebuf[idx] = linebuf[idx] .. txt
-- 	flush_output()
-- 	proc.wait_sysdt(0.5)
-- end


-- ---------------------------------------------------------------------------
-- Range monitor background task
-- (does not terminate. Run as background job and kill when done)
-- ---------------------------------------------------------------------------
function monitor_range()
	while true do
		if v1 ~= nil and v2 ~= nil then
			local v3 = vec.sub(v2:local2rel(pos2), v1:local2rel(pos1))
		  local distance = vec.length(v3)
			-- set_line(4, string.format("(distance: %.3f | x:%+.3f y:%+.3f z:%+.3f [m])", distance, v3.x, v3.y, v3.z))
			set_line(4, string.format("(distance: %.3f [m])", distance))
		end
		proc.wait_sysdt(0.1) -- 100ms refresh rate
		-- proc.skip()
	end
end


-- ---------------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------------

-- Setup
vessel1 = { name="STS-101", port=1 } -- port: [1...]
vessel2 = { name="ISS"    , port=2 }
f = 200 * 1e3 -- 200 kN (separation force)

clear_lines()
set_line(0, "=== Lua script undocking ===")

-- Get our vessel (Shuttle) and other vessel (ISS) objects
v1 = vessel.get_interface(vessel1.name)
v2 = vessel.get_interface(vessel2.name)

-- Get docking parameter (for apply force and later re-enabling)
dh1 = v1:get_dockhandle(vessel1.port-1)
dh2 = v2:get_dockhandle(vessel2.port-1)
pos1,dir1,rot1 = v1:get_dockparams(dh1)
pos2,dir2,rot2 = v1:get_dockparams(dh2)

-- Calculate force vectors
f1 = vec.mul(vec.unit(dir1), f) -- F(shuttle)
f2 = vec.mul(vec.unit(dir2), f) -- F(station)

for t=8,1,-1 do
	set_line(2, "undocking in " .. t .. " seconds ...")
	proc.wait_simdt(1)
end
set_line(2, "undocking now!")

-- Launch monitor
proc_id = proc.bg(monitor_range)

-- Delete the docking port and apply some force where it used to be...
v1:del_dock(dh1)
v1:add_force(pos1, f1)
v2:add_force(pos2, f2)

-- Re create the docking port
-- This should be done after some time and/or distance...
-- Here we just  wait 15 seconds
for t=15,1,-1 do
	set_line(3, "re-docking inhibited for " .. t .. " seconds ...")
	proc.wait_simdt(1)
end
set_line(3, "docking re-enabled again!")

v1:create_dock(pos1, dir1, rot1)

set_line(5, "=== End of script ===")

-- proc.kill(proc_id)

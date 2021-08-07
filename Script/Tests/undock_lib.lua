-- ---------------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------------
function undock (vesselName, port)

	-- check options
	if type(vesselName) ~= "string" then
		vesselName = proc.wait_input('Vessel name:');
	end

	v1 = vessel.get_interface(vesselName)
	if v1 == nil then
		error('no vessel with name "' .. vesselName .. '" found!')
		return
	end
	dockCount = v1:get_dockcount()

	if type(port) ~= "number" then
		local range = '';
		if dockCount > 1 then
			range = ' - ' .. dockCount
		end
		port = proc.wait_input('Docking port [1' .. range .. ']:');
	end

	term.out('Vessel name:' .. vesselName)
	term.out('Port:' .. port)

	-- check mandatory options
--	if type(options.title) ~= "string" then
--		error("no title")
--	elseif type(options.width) ~= "number" then
--		error("no width")
--	elseif type(options.height) ~= "number" then
--		error("no height")
--	end



	-- Setup
--	vessel1 = { name="STS-101", port=1 } -- port: [1...]
--	vessel2 = { name="ISS"    , port=2 }
--	f = 200 * 1e3 -- 200 kN (separation force)

--	clear_lines()
--	add_line("=== Lua script undocking ===")
--	add_line("")

	-- Get our vessel (Shuttle) and other vessel (ISS) objects
--	v1 = vessel.get_interface(vessel1.name)
--	v2 = vessel.get_interface(vessel2.name)

	-- Get docking parameter (for apply force and later re-enabling)
--	dh1 = v1:get_dockhandle(vessel1.port-1)
--	dh2 = v2:get_dockhandle(vessel2.port-1)
--	pos1,dir1,rot1 = v1:get_dockparams(dh1)
--	pos2,dir2,rot2 = v1:get_dockparams(dh2)

	-- Calculate force vectors
--	f1 = vec.mul(vec.unit(dir1), f) -- F(shuttle)
--	f2 = vec.mul(vec.unit(dir2), f) -- F(station)

--	add_line("undocking in 10 seconds ...")
--	proc.wait_simdt(10)
--	append_to_line("now!")

	-- Delete the docking port and apply some force where it used to be...
--	v1:del_dock(dh1)
--	v1:add_force(pos1, f1)
--	v2:add_force(pos2, f2)

	-- Re create the docking port
	-- This should be done after some time and/or distance...
	-- Here we just  wait 15 seconds
--	add_line("re-docking inhibited for 15 seconds ...")
--	proc.wait_simdt(15)
--	append_to_line("end!")

--	v1:create_dock(pos1, dir1, rot1)

--	add_line("re-docking possible again!")
--	add_line("")
--	add_line("=== End of script ===")
end
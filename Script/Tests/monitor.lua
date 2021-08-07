-- ---------------------------------------------------------------------------
-- Some useful general constants
-- ---------------------------------------------------------------------------
PI=3.14159265358979    -- pi
PI05=1.57079632679490  -- pi/2
RAD=PI/180.0           -- deg->rad conversion factor
DEG=180.0/PI           -- rad->deg conversion factor

-- ---------------------------------------------------------------------------
-- ANNOTATION SETUP
-- ---------------------------------------------------------------------------
-- re-use 1st 'old' annotation object (if available)
note = oapi.get_annotations() or oapi.create_annotation()
note:set_pos (0.2,0.1,0.8,0.9);
note:set_size(0.5)
note:set_colour ({r=0.7,g=0.8,b=1})

-- ---------------------------------------------------------------------------
-- Monitor background task
-- (does not terminate. Run as background job and kill when done)
-- ---------------------------------------------------------------------------
function monitor_range()
	while true do
    v = vessel.get_focusinterface()
		if v ~= nil then
            hobj = v:get_surfaceref()
            equ = oapi.global_to_equ (hobj, v:get_globalpos())

            name = v:get_name()
            lng = equ.lng *DEG
            lat = equ.lat *DEG

            note:set_text( string.format("lon: %.3f lat: %.3f [%s]", lng, lat, name))
        else
			note:set_text( "lon: n/a lat: n/a [n/a]" )
		end
		proc.wait_sysdt(0.1) -- 100ms refresh rate
	end
end

-- ---------------------------------------------------------------------------
-- MAIN
-- ---------------------------------------------------------------------------

-- Launch monitor process
proc_id = proc.bg(monitor_range)


-- proc.kill(proc_id)
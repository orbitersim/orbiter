-- Orbiter Lua script initialisations

-- Some useful general constants
PI=3.14159265358979    -- pi
PI05=1.57079632679490  -- pi/2
RAD=PI/180.0           -- deg->rad conversion factor
DEG=180.0/PI           -- rad->deg conversion factor
C0=299792458.0         -- speed of light in vacuum [m/s]
TAUA=499.004783806     -- light time for 1 AU [s]
AU=C0*TAUA             -- astronomical unit (mean geocentric distance of the sun) [m]
GGRAV=6.67259e-11      -- gravitational constant [m^3 kg^-1 s^-2]
G0=9.81                -- gravitational acceleration [m/s^2] at Earth mean radius
ATMP=101.4e3           -- atmospheric pressure [Pa] at Earth sea level
ATMD=1.293             -- atmospheric density [kg/m^3] at Earth sea level


-- Build a key table for orbiter keys
-- NEEDS TO BE COMPLETED!
ktable = {Q=0x10, W=0x11, E=0x12, R=0x13, T=0x14, Y=0x15, U=0x16, I=0x17,
          O=0x18, P=0x19, A=0x1E, S=0x1F, D=0x20, F=0x21, G=0x22, H=0x23,
          J=0x24, K=0x25, L=0x26, Z=0x2C, X=0x2D, C=0x2E, V=0x2F, B=0x30,
          N=0x31, M=0x32}


-- execute a script in the 'Script' folder (.lua extension is assumed)
function run (script)
  dofile('Script/'..script..'.lua')
end

-- execute a script in the Orbiter root folder
function run_global (script)
  dofile(script)
end

-- -------------------------------------------------
-- Branch management
-- Branch threads are stored in table 'branch' with
-- counter 'branch.count'
-- -------------------------------------------------

branch = {}
branch.count = 0
branch.nslot = 0

-- Create a new branch coroutine, store it in the branch
-- table, and execute its first cycle

function proc.bg (func,...)
	-- first, collect all dead branches
	for i=1,branch.nslot do
		if branch[i] ~= nil then
			if coroutine.status(branch[i]) == 'dead' then
				branch[i] = nil
				branch.count = branch.count-1
			end
		end
	end

	-- check if there is a free slot
	local slot = 0
	for i=1,branch.nslot do
		if branch[i] == nil then
			slot = i
			break
		end
	end

	-- no free slot: increase branch counter
	if slot == 0 then
		branch.nslot = branch.nslot+1
		slot = branch.nslot
	end

	-- create the new branch
	local th = coroutine.create (func)
	branch[slot] = th
	branch.count = branch.count+1
	coroutine.resume (th,...)
	term.out ('job id='..slot..' ('..branch.count..' jobs)')
	return slot
end

-- Remove a branch from the branch table (and keep fingers
-- crossed that the coroutine will be garbage-collected)

function proc.kill (n)
	if branch[n] ~= nil then
		branch[n] = nil
		branch.count = branch.count-1
		while (branch.nslot > 0) and (branch[branch.nslot] == nil) do
			branch.nslot = branch.nslot-1
		end
		term.out ('job '..n..' killed ('..branch.count..' jobs left)')
	end
end

-- Time skip: branches yield, the main trunk resumes all
-- coroutines for a single cycle, then calls proc.Frameskip
-- to pass control back to orbiter for a new simulation cycle

function proc.skip ()
	if coroutine.running() == nil then  -- we are in the main trunk
		for i=1,branch.nslot do
			if branch[i] ~= nil then
				coroutine.resume (branch[i])
			end
		end
		proc.Frameskip() -- hand control to orbiter for one cycle
            if wait_exit ~= nil then
                error()   -- return to caller immediately
            end
	else
		coroutine.yield()
	end
end

-- -------------------------------------------------
-- A few waiting functions
-- -------------------------------------------------

-- wait for simulation time t.
-- Optionally execute a function at each frame while waiting
function proc.wait_simtime (t, f, ...)
    while oapi.get_simtime() < t do
		if f then
			f(unpack(arg))
		end
		proc.skip()
	end
end

-- wait for simulation interval dt
-- Optionally execute a function at each frame while waiting
function proc.wait_simdt (dt, f, ...)
    local t1 = oapi.get_simtime()+dt
    while oapi.get_simtime() < t1 do
		if f then
			f(unpack(arg))
		end
		proc.skip()
	end
end

-- wait for system time t
-- Optionally execute a function at each frame while waiting
function proc.wait_systime (t, f, ...)
    while oapi.get_systime() < t do
		if f then
			f(unpack(arg))
		end
		proc.skip()
	end
end

-- wait for system interval dt
-- Optionally execute a function at each frame while waiting
function proc.wait_sysdt (dt, f, ...)
    local t1 = oapi.get_systime()+dt
    while oapi.get_systime() < t1 do
		if f then
			f(unpack(arg))
		end
		proc.skip()
	end
end

-- wait for a given number of frames
-- Optionally execute a function at each frame while waiting
function proc.wait_frames(n, f, ...)
	local frame = 0
	while frame < n do
		if f then
			f(unpack(arg))
		end
		frame = frame+1
		proc.skip()
	end
end

-- wait for f() >= tgt
function proc.wait_ge (f, tgt, ...)
    while f(unpack(arg)) < tgt do proc.skip() end
end

-- wait for f() <= tgt
function proc.wait_le (f, tgt, ...)
    while f(unpack(arg)) > tgt do proc.skip() end
end

-- wait for input
function proc.wait_input (title)
    oapi.open_inputbox (title)
    local ans = oapi.receive_input ()
    while ans == nil do
        proc.skip()
        ans = oapi.receive_input ()
    end
    return ans
end

-- -------------------------------------------------
-- Private functions (should not be directly called)
-- -------------------------------------------------

-- Idle loop (called after command returns, to allow
-- background jobs to continue executing)

function _idle ()
	for i=1,branch.nslot do
		if branch[i] ~= nil then
			coroutine.resume (branch[i])
		end
	end
	return branch.count
end

-- Returns the number of background jobs

function _nbranch ()
    return branch.count
end

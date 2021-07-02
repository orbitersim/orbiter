-- module help
function help ()
    term.out('Lua script demo functions')
    term.out('demo_vlist(): output a list of vessels in the current scenario.')
	term.out('demo_focus(): output the focus vessel name.')
	term.out('demo_mon(): monitors a vessel state.')
	term.out('demo_wait(): waits for a time period.')
	term.out('')
end

-- print out the name of the current focus vessel
function demo_focus ()
	term.out('Current input focus vessel:')
	term.out(focus:get_name()..'\n')
end

-- print out a list of vessels into the console
function demo_vlist ()
	term.out('Vessels in the current simulation:')
    local n = vessel.get_count()
    for i=0,n-1 do
        v = vessel.get_interface(i)
        nm = v:get_name()
        term.out (nm)
    end
	term.out('')
end

-- print continuous focus vessel altitude
function demo_mon ()
	term.out ('Demo for monitoring a parameter state.')
	v = vessel.get_focusinterface()
	term.out (v:get_name() .. ' altitude:\n')
    for i=0,1000 do
		a = v:get_altitude()
		term.lineup()
		term.out (a)
		proc.skip()
	end
	term.out('')
end

-- wait for a period of time
function demo_wait ()
	term.out ('Demo waits for a time period.')
    t = oapi.get_simtime()
	dt = 10
	term.out ('Waiting for '..dt..' seconds ...')
	proc.wait_simtime (t+dt)
	term.out ('Done!\n')
end

-- branch example
function branch_func ()
	term.out ('Branch writing to debug output...')
	t = oapi.get_simtime()+10
	while oapi.get_simtime() < t do
		oapi.dbg_out ('Branch executing: t=' .. oapi.get_simtime())
		proc.skip()
	end
end

function demo_branch ()
	term.out ('Demo creates a program branch')
	proc.CreateBranch (branch_func)
	term.out ('Back in main thread')
	proc.wait_simtime (oapi.get_simtime()+10)
end

term.out ('Script demos. Type help() for a list of functions.\n')
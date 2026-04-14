local proc = {
    tasks = {},
    nextId = 1,
    active = false
}

-- spawn a new task and run immediately
function proc.bg(func, ...)
    local co = coroutine.create(func)

    -- immediately run until first yield
    local status, res = coroutine.resume(co, ...)
    if not status then
        print("Coroutine error:", res)
		error(res)
        return nil
    end

    if coroutine.status(co) ~= "dead" then
        local id = proc.nextId
        proc.nextId = proc.nextId + 1
        table.insert(proc.tasks, { id = id, co = co })
        proc.active = true
        return id
    end

    return nil
end

-- spawn a Lua file as a task
function proc.bgFile(filename)
    local chunk, err = loadfile(filename)
    if not chunk then
        print("Failed to load file:", err)
		error(err)
        return nil
    end

    -- spawn the chunk like a normal function
    return proc.bg(chunk)
end

-- spawn a Lua string as a task
function proc.bgString(code)
    local chunk, err = loadstring(code)
    if not chunk then
        print("Failed to load string:", err)
		error(err)
        return nil
    end

    -- spawn the chunk like a normal function
    return proc.bg(chunk)
end

-- update all tasks per frame
function proc.update()
    local anyLeft = false
    local i = 1
    while i <= #proc.tasks do
        local task = proc.tasks[i]

        local status, res = coroutine.resume(task.co)
        if not status then
            print("Coroutine error:", res)
			error(res)
            table.remove(proc.tasks, i)
        elseif coroutine.status(task.co) == "dead" then
            table.remove(proc.tasks, i)
        else
            anyLeft = true
            i = i + 1
        end
    end

    proc.active = anyLeft
end

-- kill a task by ID
function proc.kill(taskId)
    for i, task in ipairs(proc.tasks) do
        if task.id == taskId then
            table.remove(proc.tasks, i)
            break
        end
    end

    proc.active = #proc.tasks > 0
end

-- check if there are any active tasks
function proc.hasTasks()
    return proc.active
end

-- abstraction for coroutine.yield()
function proc.skip()
    return coroutine.yield()
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

return proc

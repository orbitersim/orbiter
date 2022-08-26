-- read high score list from file
function hscore_read (file,n)
    local slist={}
    local f = io.open(file,"r")
    if f ~= nil then
        for i=1,n do
            local t = f:read()
            if t == nil then break end
            local name,fuel
            _,_,name,fuel = string.find (t, "(.-):(.+)")
            slist[i]={name,tonumber(fuel)}
        end
        f:close()
    end
    return slist
end

-- write high score list to file
function hscore_write (file,hscore)
    local f = io.open(file,"w")
    for i=1,#hscore do
        f:write(hscore[i][1]..':'..hscore[i][2]..'\n')
    end
    f:close()
end

-- convert high score list to a string
function hscore2str (hscore,mark)
    local str
    if #hscore > 0 then
        str = 'High score list:\n\n'..string.format('%s . . . %s', 'Fuel used', 'Name')..'\n----------------------------------'
        for i=1,#hscore do
			str = str..'\n'..string.format('%08.2f . . . %s', hscore[i][2], hscore[i][1])
            if mark==i then str = str..'   <====' end
        end
    else
        str = 'No high scores yet!'
    end
    return str
end


data_path = 'Script/Challenges/Challenge4.dat' -- high score file
max_score = 10 -- max high score entries

intro = 'Flight challenge:\n\
This sounds simple enough: Deorbit the glider and\
land at Kennedy Space Center, SLF Runway 33\
- without refuelling.\n\
Tip: a direct approach is not the best strategy here.\n'

term.out (intro)

-- read highscore list
slist = hscore_read (data_path, max_score)

-- sanity checks
v = vessel.get_interface('GL-1')
if v == nil then
    term.out ('Could not find GL-1. Aborting')
    return
elseif v:get_classname() ~= 'Deltaglider' then
    term.out ('Wrong vessel class. Aborting')
    return
end

hd = v:get_dockhandle (0)
hp0 = v:get_propellanthandle (0)  -- main tank
hp1 = v:get_propellanthandle (1)  -- RCS tank
m0 = 391.20
m1 = v:get_propellantmass (hp0) + v:get_propellantmass (hp1)
if m0 == m1 then
    ptext = string.format ("Initial propellant mass: %.2f kg\n", m0)
else
    ptext = string.format ("Initial propellant mass: %.2f kg\nUsed so far: %.2f kg\n", m0, m0-m1)
end
htext = hscore2str(slist)
term.out(ptext..'\n'..htext..'\n')

note = oapi.create_annotation()
note:set_pos (0.3,0.05,0.9,0.95)
note:set_colour ({r=1,g=0.6,b=0.2})
note:set_text (intro..'\n'..ptext..'\n'..htext)
tgt_equ = { lng = -80.694354*RAD, lat = 28.614924*RAD }

-- wait for engines
while v:get_propellantmass (hp0) + v:get_propellantmass(hp1) == m1 do
    proc.skip()
    if m1 < m0 and oapi.get_simtime() > 10 then
        break
    end
end
m1a = v:get_propellantmass (hp0)
m1b = v:get_propellantmass (hp1)
if m1a + m1b > m1 then
	note:set_text('Mission failed!')
	error('Script terminated.')
end

note:set_pos (0.05,0.2,0.5,0.5)
note:set_size (1.2)

-- wait for landing event
planet = v:is_landed()
contact = false
while planet==nil do
    m1a_new = v:get_propellantmass (hp0)
    m1b_new = v:get_propellantmass (hp1)
	if m1a_new > m1a or m1b_new > m1b then
		note:set_text('Mission failed!')
		error('Script terminated.')
	else
		m1a = m1a_new
		m1b = m1b_new
	end
	note:set_text('check')
    alt = v:get_altitude()
    if contact == false then
        contact = v:get_groundcontact()
        if contact == true then
            vel = v:get_horizonairspeedvector()
            if vel.y < -10 then
                note:set_text (string.format ("Mission failure: hard surface impact!\n(%.2f m/s)", -vel.y));
                proc.wait_sysdt(20)
                oapi.del_annotation (note)
                return
            end
        end
    elseif alt > 5 then
        contact = false
    end
    msg = string.format("Fuel remaining:\n%.2f kg (Main) + %.2f (RCS)", m1a, m1b)
    if alt < 100e3 then
	  hobj = v:get_surfaceref()
        if oapi.get_objname(hobj) == 'Earth' then
            equ = oapi.global_to_equ (hobj, v:get_globalpos())
            alpha = oapi.orthodome (equ, tgt_equ)
            r = oapi.get_size(hobj)
	      msg = msg..string.format("\n\nDistance to target: %g km\n", alpha*r*1e-3)
        end
    end

    if contact == true then
        msg = msg.."\n\nGround contact!"
    end
    note:set_text (msg)
    proc.skip()
    planet = v:is_landed()
    if planet ~= nil then -- make sure we have landed on Earth
        if oapi.get_objname(planet) ~= 'Earth' then planet = nil end
    end
end

-- write out the results
dm = m0 - (v:get_propellantmass (hp0) + v:get_propellantmass(hp1))
ptext = "Challenge completed successfully!\n"
ptext = ptext..string.format ("Total fuel expenditure: %.2f kg", dm)
note:set_pos (0.3,0.15,0.9,0.95)
note:set_size (1)
note:set_text (ptext)

-- enter high score list!
if #slist<max_score or dm < slist[#slist][2] then
	ptext = ptext.."\n\nYou made it into the high score!"
	note:set_text (ptext)
    name = proc.wait_input ('High score: Enter your name:')
    idx = #slist+1
    for i = 1,#slist do
        if slist[i][2] > dm then idx=i; break end
    end
    table.insert(slist,idx,{name,dm})
    if #slist > max_score then table.remove(slist) end
    hscore_write (data_path, slist)
end

ptext = ptext..'\n\n'..hscore2str(slist,idx)
note:set_text (ptext)
proc.wait_sysdt(20)
oapi.del_annotation (note)
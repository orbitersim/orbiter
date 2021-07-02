-- Load attitude functions
run('attctrl')

term.out('Space Shuttle Atlantis ascent autopilot')
term.out('Run function \'launch()\' to start')
term.out('Global variables (can be adjusted before launch):')
term.out('vif: target spacecraft interface (default: current)')
term.out('orbit_alt: target altitude [m] (default 300e3)')
term.out('azimuth: launch azimuth [rad] (default: 90*RAD)')

-- Reset some of the attitude parameters
pitch_a = 0.6
pitch_b = 5.7

vif = V -- target vessel interface
if vif == nil then
   vif = vessel.get_focusinterface()
end
orbit_alt = 300e3   -- target orbit altitude (can be overridden before launch)
azimuth = PI/2      -- launch azimuth
do_oms1 = true
do_oms2 = true

R_Earth = 6.37101e6 -- Earth radius
comment = nil       -- running comments
last_comment = nil  -- previous comment
comment_timer = 0

-- return periapsis radius
function ped (v)
    local el,prm
	el,prm = v:get_elementsex()
	return prm.PeD
end

-- return apoapsis radius
function apd (v)
	el,prm = v:get_elementsex()
	return prm.ApD
end

-- return apoapsis time
function apt (v)
	el,prm = vif:get_elementsex()
	return prm.ApT
end

function met_counter ()
  local t0 = oapi.get_simtime()+7
  local met, metstr
  for i=-7,10000 do
    met = oapi.get_simtime()-t0
    metstr = 'MET = '..string.format('%0.0f',met)
	if i==126 then comment = 'Solid rocket booster separation' end
	if comment ~= last_comment then
	  comment_timer = 0
	else
	  comment_timer = comment_timer+1
	  if comment_timer > 10 then comment = nil; comment_timer = 0 end
	end
	last_comment = comment
	if comment ~= nil then
	  metstr = metstr..'\n'..comment
	end
    note.set_text(metstr)
	proc.wait_simtime(t0+i+1)
  end
  note.set_text('')
end

function spdpitch (v)
  -- pitch angle of velocity marker
  -- this only works for bank=0 or bank=pi
  local p=v:get_pitch()
  local a=v:get_aoa()
  if math.abs(v:get_bank()) < PI05 then
    return p-a
  else
    return p+a
  end
end

function ascent_pitch_prog (v)
  local p = {70,60,50,40,30,20, 10,  0,-5}  -- pitch targets
  local t = {20,10,10,10,40,65,195,130,30}  -- pitch intervals
  for i = 1,7 do
    term.out ('pitch '..p[i])
	setpitch (v,RAD*p[i],t[i],0)
	-- note that we set a tolerance of 0 to prevent the
	-- pitch programme from terminating prematurely
  end
  term.out ('hold altitude programme')
  pitch_a = 1.0
  pitch_b = 3.0
  setpitch (v,0,160,0,spdpitch)
end


-- ------------------------------------------------------
-- The complete autopilot
-- ------------------------------------------------------

function launch ()
  term.out('Ascent autopilot initiated.')
  term.out('Attached to: '..vif:get_name())
  term.out('Launch azimuth: '..azimuth*DEG..'deg.')
  term.out('Target altitude: '..orbit_alt*1e-3..'km')
  if vif:get_classname() ~= 'Atlantis' then
    term.out('**** WARNING: Unexpected vessel type')
    term.out('**** '..vif:get_classname())
  end
  ascent(vif)
  if do_oms1==true then
    oms_burn1 (vif,orbit_alt)
    if do_oms2==true then
      oms_burn2 (vif,orbit_alt)
    end
  end
  term.out('Exit launch autopilot.')
end


-- ------------------------------------------------------
-- First part of ascent autopilot.
-- Convers launch to ET separation
-- ------------------------------------------------------

function ascent (v)
  proc.bg(met_counter)  -- launch MET counter

  proc.wait_simdt(3);
  v:set_thrustergrouplevel (THGROUP.MAIN, 1)
  comment = 'Main engine ignition'
  proc.wait_simdt(4) -- SSME ignition

  t0 = oapi.get_simtime()         -- set MET reference
  comment = 'We have liftoff!'

  local dr = 10+(PI/2-azimuth)*DEG*0.1
  if dr < 0 then dr=0 elseif dr > 21 then dr=21 end

  proc.wait_simtime(t0+6)
  comment = 'Start roll programme'
  SetRCS(v, RCSMODE.BANK, -1)  -- roll programme

  proc.wait_simtime(t0+6+dr)
  SetRCS(v, RCSMODE.BANK, 0)   -- end roll programme

  proc.wait_simtime(t0+21)
  term.out('initial pitch')
  SetRCS(v, RCSMODE.PITCH, 1) -- intital pitch to avoid gimbal lock
  proc.wait_simtime(t0+24)
  SetRCS(v, RCSMODE.PITCH, 0)

  proc.wait_simtime(t0+25)
  comment = 'initiate roll and yaw stabilisation'
  bank_prog = proc.bg(setbank,v,PI,500,0)
  yaw_prog  = proc.bg(setyaw,v,azimuth,500,0)
  pitch_prog = proc.bg(ascent_pitch_prog, v)
  comment = 'Start pitch programme'
  -- run the roll/yaw stabilisation and the pitch control
  -- in separate program branches

  -- meanwhile, in the main branch we sit back and wait
  -- until MECO conditions are satisfied

  proc.wait_ge(ped, R_Earth-100e3, v)
  -- wait for perigee > -100km
  proc.wait_ge(apd, R_Earth+154e3, v)
  -- wait for apogee > 154km

  proc.kill(bank_prog)
  proc.kill(yaw_prog)
  proc.kill(pitch_prog)
  -- end the ascent attitude control programs

  term.out('Main engine cut off')
  v:set_thrustergrouplevel (THGROUP.MAIN, 0)

  proc.wait_simdt(10)
  term.out('roll upright')
  setbank(v,0,40,0)
  setpitch(v,0,20,0)

  term.out('jettison tank')
  v:send_bufferedkey(ktable.J)

  proc.wait_simdt(10)
  term.out('RCS for moving away from tank')
  v:set_thrustergrouplevel (THGROUP.ATT_UP,1)
  proc.wait_simdt(2)
  v:set_thrustergrouplevel (THGROUP.ATT_UP,0)

  setpitch(v,0,10,0)
end


-- ------------------------------------------------------
-- Second part of ascent autopilot.
-- Convers OMS burn from ET separation to orbit
-- circularisation (not complete yet)
-- ------------------------------------------------------

function oms_burn1 (v,ap_alt)
  term.out('Start OMS1 programme')
  local bank_programme  = proc.bg(setbank,v,0,500,0)
  local yaw_programme   = proc.bg(setyaw,v,azimuth,500,0)
  local pitch_programme = proc.bg(setpitch,v,RAD*15,500,0)
  proc.wait_simdt (20);
  comment = 'OMS burn to raise apogee to '..ap_alt*1e-3..'km'
  term.out ('OMS burn 1 (target apogee '..ap_alt*1e-3..'km)')
  v:set_thrustergrouplevel (THGROUP.MAIN,1)
  proc.wait_ge (apd, R_Earth+ap_alt, v)
  proc.kill(bank_programme)
  proc.kill(yaw_programme)
  proc.kill(pitch_programme)
  v:set_thrustergrouplevel (THGROUP.MAIN,0)
  comment = 'OMS shutdown'
end


-- ------------------------------------------------------
-- Third part of ascent autopilot.
-- Second OMS burn for orbit circularisation
-- ------------------------------------------------------

function oms_burn2 (v,ap_alt)
  term.out ('Start OMS2 programme')
  proc.wait_le (apt, 50, v)
  comment = 'OMS burn for orbit circularisation'
  local att_programme = proc.bg(setdir,v,v.get_progradedir,0,500,0)
  proc.wait_le (apt, 20, v)
  local minalt = apd(v)-R_Earth-5e3
  v:set_thrustergrouplevel (THGROUP.MAIN,1)
  proc.wait_ge (ped, minalt+R_Earth, v)
  proc.kill(att_programme)
  v:set_thrustergrouplevel (THGROUP.MAIN,0)
  comment = 'OMS shutdown'
end

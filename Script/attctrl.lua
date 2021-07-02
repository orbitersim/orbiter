-- Lua example script: Set vessel pitch and bank angle to predefined values
-- The RCS thrust values F at any time are given by the DE
-- F = -a dq + b dv
-- where dq = q0-q is the attitude difference between preset angle q0 and
-- current angle q, and dv is the current angular velocity
-- a and b are vessel-specific parameters.

-- thrust parameters. These are adjusted for the DeltaGlider and should
-- either be overloaded for individual vessel types, or (better) be
-- calculated dynamically from vessel specs

verbose = 1      -- set to nil to suppress console output

bank_a  =  0.3
bank_b  = -2.2
pitch_a =  0.3
pitch_b =  2.3
yaw_a   = -0.3
yaw_b   =  3.2

RCSMODE = {PITCH=0,BANK=1,YAW=2}

-- activate RCS for rotation in one of the vessel's three principal axes
--   v:     vessel interface
--   mode:  one of the RCSMODE keys
--   level: thrust level (0..1)

function SetRCS (v, mode, level)
	if mode == RCSMODE.PITCH then
		if level >= 0 then
			v:set_thrustergrouplevel (THGROUP.ATT_PITCHDOWN, 0);
			v:set_thrustergrouplevel (THGROUP.ATT_PITCHUP, level);
		else
			v:set_thrustergrouplevel (THGROUP.ATT_PITCHUP, 0);
			v:set_thrustergrouplevel (THGROUP.ATT_PITCHDOWN, -level);
		end
    elseif mode == RCSMODE.BANK then
		if level >= 0 then
			v:set_thrustergrouplevel (THGROUP.ATT_BANKRIGHT, 0);
			v:set_thrustergrouplevel (THGROUP.ATT_BANKLEFT, level);
		else
			v:set_thrustergrouplevel (THGROUP.ATT_BANKLEFT, 0);
			v:set_thrustergrouplevel (THGROUP.ATT_BANKRIGHT, -level);
		end
    elseif mode == RCSMODE.YAW then
		if level >= 0 then
			v:set_thrustergrouplevel (THGROUP.ATT_YAWRIGHT, 0);
			v:set_thrustergrouplevel (THGROUP.ATT_YAWLEFT, level);
		else
			v:set_thrustergrouplevel (THGROUP.ATT_YAWLEFT, 0);
			v:set_thrustergrouplevel (THGROUP.ATT_YAWRIGHT, -level);
		end
	end
end


-- set bank angle to predefined value
--   b0:   bank angle [rad]
--   tmax: max runtime [s] (optional, default=100)
--   tol:  tolerance for angle and velocity (optional, default=1e-3)

function setbank (v,b0,tmax,tol)
  -- if bank_func == nil then bank_func=bank end
  local t0 = oapi.get_simtime()
  if tmax == nil then tmax=t0+100 else tmax=t0+tmax end
  if tol == nil then tol = 1e-3 end
  if verbose ~= nil then
    term.out('  setbank initiated ('..b0*DEG..'deg, tmax='..tmax-t0..', tol='..tol..')')
  end

  repeat
    av = v:get_angvel() -- angular velocity
    b = v:get_bank()     -- current bank value
    vb = av.z           -- banking velocity (damping term)
    db = b0-b           -- angle difference
    if db > PI then db = db - 2*PI elseif db < -PI then db = db + 2*PI end
    rcs = db*bank_a - vb*bank_b
    res_db = math.abs(db)        -- angle residual
    res_vb = math.abs(vb)        -- velocity residual
    res = math.max(res_db,res_vb)
    if rcs > 1 then rcs = 1 elseif rcs < -1 then rcs = -1 end
    if res < 1e-3 then rcs = rcs*0.1 end
    if res < 1e-4 then rcs = rcs*0.1 end
                     -- close to minimum, slow down for higher accuracy
    SetRCS(v, RCSMODE.BANK, rcs)
    if (res_db > tol or res_vb > tol) and oapi.get_simtime() < tmax then
	  proc.skip()
	end
  until res_db <= tol and res_vb <= tol or oapi.get_simtime() >= tmax
  SetRCS(v, RCSMODE.BANK, 0)

  if verbose ~= nil then
    dt = oapi.get_simtime()-t0
    if dt > tmax-t0 then
      term.out('  setbank timed out (t='..string.format('%0.2f',dt)..')')
    else
      term.out('  setbank converged (t='..string.format('%0.2f',dt)..')')
    end
    term.out('  residuals: angle='..string.format('%0.6g',db)..', vel='..string.format('%0.6g',vb))
  end
end


-- set pitch angle to predefined value
--   p0:   pitch angle [rad]
--   tmax: max runtime [s] (optional, default=100)
--   tol:  tolerance for angle and velocity (optional, default=1e-3)
-- BUGS:
--   1. crossing poles causes problems due to pitch angle wrap
--   2. target velocity of 0 not necessary correct in orbit, therefore fails to
--      converge

function setpitch (v,p0,tmax,tol,pitch_func)
  if pitch_func == nil then pitch_func=v.get_pitch end
  local t0 = oapi.get_simtime()
  if tmax == nil then tmax=t0+100 else tmax=t0+tmax end
  if tol == nil then tol = 1e-3 end
  if verbose ~= nil then
    term.out('  setpitch initiated ('..p0*DEG..'deg, tmax='..tmax-t0..', tol='..tol..')')
  end

  repeat
    av = v:get_angvel() -- angular velocity
    p = pitch_func(v)   -- current pitch value
    vp = av.x           -- pitch velocity (damping term)
    dp = p0-p           -- angle difference
    if dp > PI then dp = dp - 2*PI elseif dp < -PI then dp = dp + 2*PI end
    if math.abs(v:get_bank()) > PI05 then dp=-dp end
    -- reverse direction if banked more than 90 deg
    rcs = dp*pitch_a - vp*pitch_b
    res_dp = math.abs(dp)        -- angle residual
    res_vp = math.abs(vp)        -- velocity residual
    res = math.max(res_dp,res_vp)
    if rcs > 1 then rcs = 1 elseif rcs < -1 then rcs = -1 end
    if res < 1e-3 then rcs = rcs*0.1 end
    if res < 1e-4 then rcs = rcs*0.1 end
                     -- close to minimum, slow down for higher accuracy
    SetRCS(v, RCSMODE.PITCH, rcs)
    if (res_dp > tol or res_vp > tol) and oapi.get_simtime() < tmax then
	  proc.skip()
	end
  until res_dp < tol and res_vp < tol or oapi.get_simtime() > tmax
  SetRCS(v, RCSMODE.PITCH, 0)

  if verbose ~= nil then
    dt = oapi.get_simtime()-t0
    if dt > tmax-t0 then
      term.out('  setpitch timed out (t='..string.format('%0.2f',dt)..')')
    else
      term.out('  setpitch converged (t='..string.format('%0.2f',dt)..')')
    end
    term.out('  residuals: angle='..string.format('%0.6g',dp)..', vel='..string.format('%0.6g',vp))
  end
end


-- set yaw angle to predefined value
--   y0:   yaw angle [rad]
--   tmax: max runtime [s] (optional, default=100)
--   tol:  tolerance for angle and velocity (optional, default=1e-3)

function setyaw (v,y0,tmax,tol,yaw_func)
  if yaw_func == nil then yaw_func=v.get_yaw end
  local t0 = oapi.get_simtime()
  if tmax == nil then tmax=t0+100 else tmax=t0+tmax end
  if tol == nil then tol = 1e-3 end
  if verbose ~= nil then
    term.out('  setyaw initiated ('..y0*DEG..'deg, tmax='..tmax-t0..', tol='..tol..')')
  end

  repeat
	av = v:get_angvel() -- angular velocity
    y = yaw_func(v)     -- current yaw value
    vy = av.y           -- yaw velocity (damping term)
    dy = y0-y           -- angle difference
    if dy > PI then dy = dy - 2*PI elseif dy < -PI then dy = dy + 2*PI end
    if math.abs(v:get_bank()) > PI05 then dy=-dy end
    -- reverse direction if banked more than 90 deg
    rcs = dy*yaw_a - vy*yaw_b
    res_dy = math.abs(dy)        -- angle residual
    res_vy = math.abs(vy)        -- velocity residual
    res = math.max(res_dy,res_vy)
    if rcs > 1 then rcs = 1 elseif rcs < -1 then rcs = -1 end
    if res < 1e-3 then rcs = rcs*0.1 end
    if res < 1e-4 then rcs = rcs*0.1 end
                     -- close to minimum, slow down for higher accuracy
    SetRCS(v, RCSMODE.YAW, rcs)
    if (res_dy > tol or res_vy > tol) and oapi.get_simtime() < tmax then
	  proc.skip()
	end
  until res_dy < tol and res_vy < tol or oapi.get_simtime() > tmax
  SetRCS(v, RCSMODE.YAW, 0)

  if verbose ~= nil then
    dt = oapi.get_simtime()-t0
    if dt > tmax-t0 then
      term.out('  setyaw timed out (t='..string.format('%0.2f',dt)..')')
    else
      term.out('  setyaw converged (t='..string.format('%0.2f',dt)..')')
    end
    term.out('  residuals: angle='..string.format('%0.6g',dy)..', vel='..string.format('%0.6g',vy))
  end
end


function setdir (v,dir_func,b0,tmax,tol)
  local t0 = oapi.get_simtime()
  if b0 == nil then b0=0 end
  if tmax == nil then tmax=t0+100 else tmax=t0+tmax end
  if tol == nil then tol = 1e-3 end
  if verbose ~= nil then
    term.out('  setprograde initiated (bank='..b0*DEG..'deg, tmax='..tmax-t0..', tol='..tol..')')
  end

  repeat
	av = v:get_angvel()
	local p = dir_func(v)
	bk = v:get_bank()
	dp = math.atan2(p.y,p.z)
	dy = math.atan2(p.x,p.z)
	vp = av.x
	vy = av.y
    vb = av.z        -- banking velocity (damping term)
    rcs = dp*pitch_a - vp*pitch_b
    res_dp = math.abs(dp)        -- angle residual
    res_vp = math.abs(vp)        -- velocity residual
    res = math.max(res_dp,res_vp)
    if rcs > 1 then rcs = 1 elseif rcs < -1 then rcs = -1 end
    if res < 1e-3 then rcs = rcs*0.1 end
    if res < 1e-4 then rcs = rcs*0.1 end
                     -- close to minimum, slow down for higher accuracy
    SetRCS(v, RCSMODE.PITCH, rcs)

    rcs = dy*yaw_a - vy*yaw_b
    res_dy = math.abs(dy)        -- angle residual
    res_vy = math.abs(vy)        -- velocity residual
    res = math.max(res_dy,res_vy)
    if rcs > 1 then rcs = 1 elseif rcs < -1 then rcs = -1 end
    if res < 1e-3 then rcs = rcs*0.1 end
    if res < 1e-4 then rcs = rcs*0.1 end
                     -- close to minimum, slow down for higher accuracy
    SetRCS(v, RCSMODE.YAW, rcs)

	db = b0-bk
    if db > PI then db = db - 2*PI elseif db < -PI then db = db + 2*PI end
    rcs = db*bank_a - vb*bank_b
    res_db = math.abs(db)        -- angle residual
    res_vb = math.abs(vb)        -- velocity residual
    res = math.max(res_db,res_vb)
    if rcs > 1 then rcs = 1 elseif rcs < -1 then rcs = -1 end
    if res < 1e-3 then rcs = rcs*0.1 end
    if res < 1e-4 then rcs = rcs*0.1 end
                     -- close to minimum, slow down for higher accuracy
    SetRCS(v, RCSMODE.BANK, rcs)

    if (res_dp > tol or res_vp > tol) and oapi.get_simtime() < tmax then
	  proc.skip()
	end
  until res_dp < tol and res_vp < tol or oapi.get_simtime() > tmax
  SetRCS(v, RCSMODE.PITCH, 0)

  if verbose ~= nil then
    dt = oapi.get_simtime()-t0
    if dt > tmax-t0 then
      term.out('  setdir timed out (t='..string.format('%0.2f',dt)..')')
    else
      term.out('  setdir converged (t='..string.format('%0.2f',dt)..')')
    end
    term.out('  residuals: angle='..string.format('%0.6g',dp)..', vel='..string.format('%0.6g',vp))
  end
end

-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: GimbalControl.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local DGSubsystem = require("DGSubsystem")
local MainGimbalDial = require("MainGimbalDial")
local PMainGimbalCtrl = require("PMainGimbalCtrl")
local YMainGimbalCtrl = require("YMainGimbalCtrl")
local MainGimbalDisp = require("MainGimbalDisp")

local mesh_vc = require("meshres_vc")
local GRP_VC = mesh_vc.GRP

local MAIN_PGIMBAL_RANGE = math.tan (5.0*RAD)
local MAIN_YGIMBAL_RANGE = 1.0/7.7
-- main engine pitch and yaw gimbal range (tan)

local MAIN_GIMBAL_SPEED = 0.06
-- operating speed of main engine pitch and yaw gimbals

-- ==============================================================
-- Main engine gimbal control
-- ==============================================================
local GimbalControl = Class(DGSubsystem)

function GimbalControl:new (_subsys)
	DGSubsystem.new(self, _subsys)
	self.mode = 0
	self.mpmode = 0
	self.mymode = 0

	self.mpgimbal = {0,0}
	self.mpgimbal_cmd = {0,0}
	self.mygimbal = {0,0}
	self.mygimbal_cmd = {0,0}
	self.mpswitch = {0,0}
	self.myswitch = {0,0}

	self.ELID_MODEDIAL,      self.modedial      = self:AddElement (MainGimbalDial (self))
	self.ELID_PGIMBALSWITCH, self.pgimbalswitch = self:AddElement (PMainGimbalCtrl (self))
	self.ELID_YGIMBALSWITCH, self.ygimbalswitch = self:AddElement (YMainGimbalCtrl (self))
	self.ELID_DISPLAY,       self.gimbaldisp    = self:AddElement (MainGimbalDisp (self))
end

--------------------------------------------------------------

function GimbalControl:SetMainPGimbal (which, lvl)
	local dir = self:DG():GetMainThrusterDir (which)
	dir.x = dir.x / dir.z
	dir.z = 1
	dir.y = MAIN_PGIMBAL_RANGE*lvl
	self.mpgimbal[which] = dir.y
	self:DG():SetMainThrusterDir (which, vec.unit(dir))
end

--------------------------------------------------------------

function GimbalControl:SetMainYGimbal (which, lvl)
	local dir = self:DG():GetMainThrusterDir (which)
	dir.y = dir.y / dir.z
	dir.z = 1
	dir.x = MAIN_YGIMBAL_RANGE*lvl
	self.mygimbal[which] = dir.x
	self:DG():SetMainThrusterDir (which, vec.unit(dir))
end

--------------------------------------------------------------

function GimbalControl:IncMainPGimbal (which, dir)
	local cmd_speed = 0.5
	local dcmd = oapi.get_simstep() * cmd_speed * MAIN_PGIMBAL_RANGE * (dir == 1 and -1.0 or 1.0)

	if dir == 0 then
		self.mpswitch = {0,0}
	else
		for i=1,2 do
			if bit.anyset(which, i) then
				if self.mode == 2 then
					self.mpgimbal_cmd[i] = math.min (MAIN_PGIMBAL_RANGE, math.max (-MAIN_PGIMBAL_RANGE, self.mpgimbal_cmd[i]+dcmd))
				end
				self.mpswitch[i] = 3 - dir
			else
				self.mpswitch[i] = 0
			end
		end
	end
	return true
end

--------------------------------------------------------------

function GimbalControl:IncMainYGimbal (which, dir)
	local cmd_speed = 0.5
	local dcmd = oapi.get_simstep() * cmd_speed * MAIN_YGIMBAL_RANGE * (dir == 1 and -1.0 or 1.0)

	if dir == 0 then
		self.myswitch = {0,0}
	else
		for i=1,2 do
			if bit.anyset(which, i) then
				if self.mode == 2 then
					self.mygimbal_cmd[i] = math.min (MAIN_YGIMBAL_RANGE, math.max (-MAIN_YGIMBAL_RANGE, self.mygimbal_cmd[i]+dcmd))
				end
				self.myswitch[i] = 3 - dir
			else
				self.myswitch[i] = 0
			end
		end
	end
	return true
end

--------------------------------------------------------------

function GimbalControl:AutoMainGimbal ()
	-- Pitch gimbal
	-- a) pitch command
	local lvl = self:DG():get_manualcontrollevel(THGROUP.ATT_PITCHDOWN, MANCTRL.ROTMODE)
	if lvl == 0.0 then
		lvl = -self:DG():get_manualcontrollevel(THGROUP.ATT_PITCHUP, MANCTRL.ROTMODE)
	end
	local plvl = {lvl, lvl}

	-- b) roll command
	lvl = self:DG():get_manualcontrollevel(THGROUP.ATT_BANKRIGHT, MANCTRL.ROTMODE)
	if lvl == 0.0 then
		lvl = -self:DG():get_manualcontrollevel(THGROUP.ATT_BANKLEFT, MANCTRL.ROTMODE)
	end
	plvl[1] = plvl[1] + lvl
	plvl[2] = plvl[2] - lvl

	-- scale to range and apply
	local mlvl = math.max(math.abs(plvl[1]), math.abs(plvl[2]))
	if mlvl > 1.0 then
		plvl[1] = plvl[1] / mlvl
		plvl[2] = plvl[2] / mlvl
	end

	self.mpgimbal_cmd[1] = plvl[1]*MAIN_PGIMBAL_RANGE
	self.mpgimbal_cmd[2] = plvl[2]*MAIN_PGIMBAL_RANGE

	-- Yaw gimbal
	-- a) compensate for main thrust differences
	local t0 = self:DG():GetMainThrusterLevel (1)
	local t1 = self:DG():GetMainThrusterLevel (2)
	local tt = t0+t1
	mlvl = tt ~= 0.0 and (t0-t1)/tt or 0.0
	
	-- b) yaw command
	lvl = self:DG():get_manualcontrollevel(THGROUP.ATT_YAWLEFT, MANCTRL.ROTMODE)
	if lvl == 0.0 then
		lvl = -self:DG():get_manualcontrollevel(THGROUP.ATT_YAWRIGHT, MANCTRL.ROTMODE)
	end
	mlvl = mlvl + lvl

	-- scale to range and apply
	mlvl = math.min (1.0, math.max (-1.0, mlvl))
	self.mygimbal_cmd[1] = mlvl*MAIN_YGIMBAL_RANGE
	self.mygimbal_cmd[2] = mlvl*MAIN_YGIMBAL_RANGE
end

--------------------------------------------------------------

function GimbalControl:TrackMainGimbal ()
	local update = false
	local dphi = oapi.get_simstep()*MAIN_GIMBAL_SPEED
	for i=1,2 do
		if self.mpgimbal[i] ~= self.mpgimbal_cmd[i] then
			update = true
			if self.mpgimbal[i] < self.mpgimbal_cmd[i] then
				self.mpgimbal[i] = math.min (self.mpgimbal[i]+dphi, self.mpgimbal_cmd[i])
			else
				self.mpgimbal[i] = math.max (self.mpgimbal[i]-dphi, self.mpgimbal_cmd[i])
			end
		end
		if self.mygimbal[i] ~= self.mygimbal_cmd[i] then
			update = true
			if self.mygimbal[i] < self.mygimbal_cmd[i] then
				self.mygimbal[i] = math.min (self.mygimbal[i]+dphi, self.mygimbal_cmd[i])
			else
				self.mygimbal[i] = math.max (self.mygimbal[i]-dphi, self.mygimbal_cmd[i])
			end
		end
	end
	if update then
		for i=1,2 do
			--local dir = self:DG():GetMainThrusterDir (i)
			--dir /= dir.z;
			--dir.y = mpgimbal[i]
			--dir.x = mygimbal[i]
			local dir = _V(self.mygimbal[i], self.mpgimbal[i], 1)
			self:DG():SetMainThrusterDir (i, vec.unit(dir))
		end
		self:DG():trigger_redrawarea (0, 0, self.ELID_DISPLAY)
	end
end

--------------------------------------------------------------

function GimbalControl:clbkSaveState (scn)
	if self.mode ~= 0 then
		if self.mode == 1 then -- auto
			oapi.writescenario_int (scn, "MGIMBALMODE", self.mode);
		else -- manual
			local cbuf = string.format("%d %0.3f %0.3f %0.3f %0.3f",
				self.mode, self.mpgimbal_cmd[1], self.mpgimbal_cmd[2], self.mygimbal_cmd[1], self.mygimbal_cmd[2])
			oapi.writescenario_string (scn, "MGIMBALMODE", cbuf)
		end
	end
end

--------------------------------------------------------------

function GimbalControl:clbkParseScenarioLine (line)
	local match = {}
	if scenario_line_match(line, "MGIMBALMODE %d %f %f %f %f", match) then
		self.mode = match.res[1]
		self.mpgimbal_cmd[1] = match.res[2]
		self.mpgimbal_cmd[2] = match.res[3]
		self.mygimbal_cmd[1] = match.res[4]
		self.mygimbal_cmd[2] = match.res[5]
		return true
	elseif scenario_line_match(line, "MGIMBALMODE %d", match) then
		self.mode = match.res[1]
		return true
	end
	return false
end

--------------------------------------------------------------

function GimbalControl:clbkPostStep (simt, simdt, mjd)
	if self.mode == 1 then
		self:AutoMainGimbal()
	end
	self:TrackMainGimbal()
end

--------------------------------------------------------------

function GimbalControl:clbkLoadPanel2D (panelid, hPanel, viewW, viewH)
	if panelid ~= 0 then return false end

	local panel2dtex = oapi.get_texturehandle(self:DG().panelmesh0,1)

	-- Gimbal control dial
	self:DG():register_panelarea (hPanel, self.ELID_MODEDIAL,      _R(203,426,243,470), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN, panel2dtex, self.modedial)

	-- Gimbal manual switches
	self:DG():register_panelarea (hPanel, self.ELID_PGIMBALSWITCH, _R(285,433,320,477), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP, panel2dtex, self.pgimbalswitch)
	self:DG():register_panelarea (hPanel, self.ELID_YGIMBALSWITCH, _R(280,502,324,537), PANEL_REDRAW.MOUSE,  PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP, panel2dtex, self.ygimbalswitch)

	-- Gimbal display
	self:DG():register_panelarea (hPanel, self.ELID_DISPLAY,       _R(  0,  0,  0,  0), PANEL_REDRAW.USER,   PANEL_MOUSE.IGNORE, panel2dtex, self.gimbaldisp);

	return true
end

--------------------------------------------------------------

-- GIMBAL_DIAL (VC): mouse catch area
local VC_GIMBAL_DIAL_mousearea = {_V(-0.27440,1.00130,7.19359),_V(-0.24440,1.00130,7.19359),_V(-0.27440,1.02568,7.21107),_V(-0.24440,1.02568,7.21107)}

-- GIMBAL_DIAL (VC): rotation reference
local VC_GIMBAL_DIAL_ref = _V(-0.25940,1.01349,7.20233)

-- GIMBAL_DIAL (VC): rotation axis
local VC_GIMBAL_DIAL_axis = _V(0.00000,-0.58280,0.81262)

local VC_GIMBAL_DIAL_vofs = 76

-- GIMBAL_PSWITCH (VC): mouse catch area
local VC_GIMBAL_PSWITCH_mousearea = {_V(-0.22334,1.00616,7.19708),_V(-0.20350,1.00616,7.19708),_V(-0.22334,1.02521,7.21074),_V(-0.20350,1.02521,7.21074)}

-- GIMBAL_PSWITCH (VC): rotation reference
local VC_GIMBAL_PSWITCH_ref = _V(-0.21342,1.01432,7.20581)

-- GIMBAL_PSWITCH (VC): rotation axis
local VC_GIMBAL_PSWITCH_axis = _V(1.00000,0.00000,0.00000)

-- GIMBAL_YSWITCH (VC): mouse catch area
local VC_GIMBAL_YSWITCH_mousearea = {_V(-0.22514,0.99298,7.18762),_V(-0.22514,0.97686,7.17606),_V(-0.20170,0.99298,7.18762),_V(-0.20170,0.97686,7.17606)}

-- GIMBAL_YSWITCH (VC): rotation reference
local VC_GIMBAL_YSWITCH_ref = _V(-0.21342,0.98355,7.18374)

-- GIMBAL_YSWITCH (VC): rotation axis
local VC_GIMBAL_YSWITCH_axis = _V(0.00000,-0.81262,-0.58280)

function GimbalControl:clbkLoadVC (vcid)
	if vcid ~= 0 then return false end

	-- Gimbal control dial
	oapi.VC_register_area (self.ELID_MODEDIAL, PANEL_REDRAW.USER + PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_MODEDIAL, VC_GIMBAL_DIAL_mousearea[1], VC_GIMBAL_DIAL_mousearea[2], VC_GIMBAL_DIAL_mousearea[3], VC_GIMBAL_DIAL_mousearea[4])
	self.modedial:DefineAnimationVC (VC_GIMBAL_DIAL_ref, VC_GIMBAL_DIAL_axis, GRP_VC.DIAL1, VC_GIMBAL_DIAL_vofs)

	-- Gimbal manual switches
	oapi.VC_register_area (self.ELID_PGIMBALSWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_PGIMBALSWITCH, VC_GIMBAL_PSWITCH_mousearea[1], VC_GIMBAL_PSWITCH_mousearea[2], VC_GIMBAL_PSWITCH_mousearea[3], VC_GIMBAL_PSWITCH_mousearea[4])
	oapi.VC_register_area (self.ELID_YGIMBALSWITCH, PANEL_REDRAW.MOUSE, PANEL_MOUSE.LBDOWN + PANEL_MOUSE.LBPRESSED + PANEL_MOUSE.LBUP)
	oapi.VC_set_areaclickmode_quadrilateral (self.ELID_YGIMBALSWITCH, VC_GIMBAL_YSWITCH_mousearea[1], VC_GIMBAL_YSWITCH_mousearea[2], VC_GIMBAL_YSWITCH_mousearea[3], VC_GIMBAL_YSWITCH_mousearea[4])

	-- Gimbal status display
	oapi.VC_register_area (self.ELID_DISPLAY, PANEL_REDRAW.ALWAYS, PANEL_MOUSE.IGNORE)

	return true
end

function GimbalControl:Mode()
	return self.mode
end

function GimbalControl:SetMode(newmode)
	self.mode = newmode
end

function GimbalControl:MainPGimbal(which, actual)
	if actual == nil then
		actual = true
	end
	return actual and self.mpgimbal[which] or self.mpgimbal_cmd[which]
end

function GimbalControl:MainYGimbal(which, actual)
	if actual == nil then
		actual = true
	end
	return actual and self.mygimbal[which] or self.mygimbal_cmd[which]
end

return GimbalControl

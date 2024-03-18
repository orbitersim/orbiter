-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License
local meshres_vc = require("meshres_vc")
local GRP_VC = meshres_vc.GRP
local plbay = {}

local DOOR_OPERATING_SPEED = 0.007353
-- Opening/closing speed of payload bay doors (1/sec)
-- This contains the door opening sequence (63 sec for each door) and an
-- interval of 10 sec between the two door operations

local RAD_OPERATING_SPEED = 0.025
-- Deployment/stowing speed of radiators (1/sec)
-- => radiator cycle = 40 sec

local RADLATCH_OPERATING_SPEED = 0.2
-- Release/engaging speed of radiator latches (1/sec)
-- => radiator latch cycle = 5 sec

local KU_OPERATING_SPEED = 0.0435
-- Deployment speed of the Ku Band antenna (1/sec)
-- cycle is 23 sec

local AID_R13L       = 100
local AID_R13L_TKBK1 = 101
local AID_R13L_TKBK2 = 102
local AID_R13L_TKBK3 = 103
local AID_R13L_TKBK4 = 104
local AID_R13L_TKBK5 = 105
local AID_R13L_TKBK6 = 106

--enums
local BD_ENABLE = 0
local BD_DISABLE = 1

local BDO_OPEN = 0
local BDO_STOP = 1
local BDO_CLOSE = 2

local MP_ON = 0
local MP_OFF = 1

local LC_RELEASE = 0
local LC_OFF = 1
local LC_LATCH = 2

local RC_DEPLOY = 0
local RC_OFF = 1
local RC_STOW = 2

local KU_DEPLOY = 0
local KU_GND = 1
local KU_STOW = 2

local KU_DIRECT_ON = 0
local KU_DIRECT_OFF = 1

local ActionString = {
	[0] = "STOPPED",
	[1] = "ISCLOSED",
	[2] = "ISOPEN",
	[3] = "CLOSE",
	[4] = "OPEN"
}

-- ==============================================================

local sts
local BayDoor
local BayDoorOp
local BayDoorStatus
local MechPwr
local RadiatorCtrl
local RadLatchCtrl
local RadiatorStatus
local RadLatchStatus
local KuCtrl
local KuDirectCtrl
local KuAntennaStatus
local anim_VC_R13L = {}

local function init(vessel)
	sts = vessel

	-- Cargo bay doors
	BayDoor = {BD_DISABLE, BD_DISABLE}
	BayDoorOp = BDO_STOP
	BayDoorStatus = animstate(0, DOOR_OPERATING_SPEED, animstate.CLOSED)

	-- Radiators
	MechPwr = { MP_OFF, MP_OFF }
	RadiatorCtrl = { RC_OFF, RC_OFF }
	RadLatchCtrl = { LC_OFF, LC_OFF }

	RadiatorStatus = animstate(0, RAD_OPERATING_SPEED, animstate.CLOSED)
	RadLatchStatus = animstate(0, RADLATCH_OPERATING_SPEED, animstate.CLOSED)

	-- Ku-band antenna
	KuCtrl = KU_GND
	KuDirectCtrl = KU_DIRECT_OFF

	KuAntennaStatus = animstate(0, KU_OPERATING_SPEED, animstate.CLOSED)
end

-- ==============================================================

local function step(t, dt)
	-- Operate cargo doors
	if BayDoorStatus:moving() then
		local da = dt * DOOR_OPERATING_SPEED
		if BayDoorStatus.status == animstate.CLOSING then
			if BayDoorStatus.proc > 0.0 then
				BayDoorStatus.proc = math.max(0.0, BayDoorStatus.proc-da)
			else
				set_door_action(animstate.CLOSED)
			end
		else -- door opening
			if BayDoorStatus.proc < 1.0 then
				BayDoorStatus.proc = math.min(1.0, BayDoorStatus.proc+da)
			else
				set_door_action(animstate.OPEN)
			end
		end
		sts.set_bay_door_position(BayDoorStatus.proc)
	end

	-- Operate radiators
	if RadiatorStatus:moving() then
		local da = dt * RAD_OPERATING_SPEED
		if RadiatorStatus.status == animstate.CLOSING then
			if RadiatorStatus.proc > 0.0 then
				RadiatorStatus.proc = math.max(0.0, RadiatorStatus.proc-da)
			else
				set_radiator_action(animstate.CLOSED)
			end
		else -- radiator deploying
			if RadiatorStatus.proc < 1.0 then
				RadiatorStatus.proc = math.min(1.0, RadiatorStatus.proc+da)
			else
				set_radiator_action(animstate.OPEN)
			end
		end
		sts.set_radiator_position(RadiatorStatus.proc)
	end

	-- Operate radiator latches
	if RadLatchStatus:moving() then
		local da = dt * RADLATCH_OPERATING_SPEED
		if RadLatchStatus.status == animstate.CLOSING then
			if RadLatchStatus.proc > 0.0 then
				RadLatchStatus.proc = math.max(0.0, RadLatchStatus.proc-da)
			else
				set_rad_latch_action(animstate.CLOSED)
			end
		else -- radiator latches releasing
			if RadLatchStatus.proc < 1.0 then
				RadLatchStatus.proc = math.min(1.0, RadLatchStatus.proc+da)
			else
				set_rad_latch_action(animstate.OPEN)
			end
		end
		--sts.SetRadLatchPosition(RadLatchStatus.proc)
	end

	-- Operate Ku-band antenna
	if KuAntennaStatus:moving() then
		local da = dt * KU_OPERATING_SPEED
		if KuAntennaStatus.status == animstate.CLOSING then
			if KuAntennaStatus.proc > 0.0 then
				KuAntennaStatus.proc = math.max(0.0, KuAntennaStatus.proc-da)
			else
				set_Ku_antenna_action(animstate.CLOSED)
			end
		else -- antenna deploying
			if KuAntennaStatus.proc < 1.0 then
				KuAntennaStatus.proc = math.min(1.0, KuAntennaStatus.proc+da)
			else
				set_Ku_antenna_action(animstate.OPEN)
			end
		end
		sts.set_Ku_antenna_position(KuAntennaStatus.proc)
	end
end

-- ==============================================================

function set_door_action(action, simple)
	if KuAntennaStatus.status ~= animstate.CLOSED then return end
	-- operate payload bay doors only if Ku-band antenna is stowed
	if RadiatorStatus.status ~= animstate.CLOSED then return end
	-- operate payload bay doors only if radiators are stowed

	if simple then
		BayDoor = {BD_ENABLE, BD_ENABLE}
	end
	-- Make sure both systems are online

	if action ~= animstate.STOPPED and BayDoor[1] ~= BD_ENABLE then return end
	if action ~= animstate.STOPPED and BayDoor[2] ~= BD_ENABLE then return end
	-- operate doors only if both systems are enabled

	if action == animstate.STOPPED and not BayDoorStatus:moving() then return end
	-- stopping doesn't make sense if the doors are already fully open or closed

	BayDoorStatus.status = action
	if action == animstate.CLOSED then
		BayDoorStatus.proc = 0.0
		sts.set_bay_door_position(0.0)
	elseif action == animstate.OPEN then
		BayDoorStatus.proc = 1.0
		sts.set_bay_door_position(1.0)
	end
	vi:record_event("CARGODOOR", ActionString[action])

	update_VC()
end

-- ==============================================================

function set_radiator_action(action)
	if BayDoorStatus.status ~= animstate.OPEN then return end
	-- allow radiator operation only once the bay doors are fully open
	if action ~= animstate.STOPPED and MechPwr[1] ~= MP_ON then return end
	if action ~= animstate.STOPPED and MechPwr[2] ~= MP_ON then return end
	-- operate radiators only if power is online

	if action == animstate.OPENING and RadiatorCtrl[1] ~= RC_DEPLOY then return end
	if action == animstate.CLOSING and RadiatorCtrl[1] ~= RC_STOW then return end
	if action == animstate.OPENING and RadiatorCtrl[2] ~= RC_DEPLOY then return end
	if action == animstate.CLOSING and RadiatorCtrl[2] ~= RC_STOW then return end

	if action == animstate.STOPPED and not RadiatorStatus:moving() then return end
	-- stopping doesn't make sense if the radiators are already fully deployed or stowed

	if action == animstate.OPENING and RadiatorStatus.status == animstate.CLOSED and RadLatchStatus.status ~= animstate.OPEN then return end
	-- don't deploy radiators if the latches are not fully released

	RadiatorStatus.status = action
	vi:record_event("RADIATOR", ActionString[action])

	update_VC()
end

-- ==============================================================

function revert_door_action()
	set_door_action((BayDoorStatus.status == animstate.CLOSED or BayDoorStatus.status == animstate.CLOSING) and
		animstate.OPENING or animstate.CLOSING, true)
end

-- ==============================================================

function set_rad_latch_action(action)
	if action ~= animstate.STOPPED and MechPwr[1] ~= MP_ON then return end
	if action ~= animstate.STOPPED and MechPwr[2] ~= MP_ON then return end
	-- operate radiator latches only if power is online

	-- check both systems are set correctly
	if action == animstate.OPENING and RadLatchCtrl[1] ~= LC_RELEASE then return end
	if action == animstate.CLOSING and RadLatchCtrl[1] ~= LC_LATCH then return end
	if action == animstate.OPENING and RadLatchCtrl[2] ~= LC_RELEASE then return end
	if action == animstate.CLOSING and RadLatchCtrl[2] ~= LC_LATCH then return end

	if action == animstate.STOPPED and not RadLatchStatus:moving() then return end
	-- stopping doesn't make sense if the radiators are already fully deployed or stowed

	RadLatchStatus.status = action
	vi:record_event("RADLATCH", ActionString[action])

	update_VC()
end

-- ==============================================================

function set_Ku_antenna_action(action)
	if BayDoorStatus.status ~= animstate.OPEN then return end
	-- allow radiator operation only once the bay doors are fully open

	if action == animstate.STOPPED and not KuAntennaStatus:moving() then return end
	-- stopping doesn't make sense if the doors are already fully open or closed

	KuAntennaStatus.status = action
	vi:record_event("KUBAND", ActionString[action])

	update_VC()
end

-- ==============================================================

function revert_Ku_antenna_action()
	set_Ku_antenna_action(KuAntennaStatus.status == animstate.CLOSED or KuAntennaStatus.status == animstate.CLOSING and
		animstate.OPENING or animstate.CLOSING)
end

-- ==============================================================

function parse_scenario_line(line)
	local match = {}
	if scenario_line_match(line, "CARGODOOR %d %f", match) then
		BayDoorStatus.status = match.res[1] + 1
		BayDoorStatus.proc = match.res[2]
		return true
	elseif scenario_line_match(line, "RADIATOR %d %f", match) then
		RadiatorStatus.status = match.res[1] + 1
		RadiatorStatus.proc = match.res[2]
		return true
	elseif scenario_line_match(line, "RADLATCH %d %f", match) then
		RadLatchStatus.status = match.res[1] + 1
		RadLatchStatus.proc = match.res[2]
		return true
	elseif scenario_line_match(line, "KUBAND %d %f", match) then
		KuAntennaStatus.status = match.res[1] + 1
		KuAntennaStatus.proc = match.res[2]
		return true
	end
	return false
end

-- ==============================================================

function save_state(scn)
	if BayDoorStatus.status ~= animstate.CLOSED then
		oapi.writescenario_string(scn, "CARGODOOR", tostring(BayDoorStatus.status-1).." "..tostring(BayDoorStatus.proc))
	end
	if RadiatorStatus.status ~= animstate.CLOSED then
		oapi.writescenario_string(scn, "RADIATOR", tostring(RadiatorStatus.status-1).." "..tostring(RadiatorStatus.proc))
	end
	if RadLatchStatus.status ~= animstate.CLOSED then
		oapi.writescenario_string(scn, "RADLATCH", tostring(RadLatchStatus.status-1).." "..tostring(RadLatchStatus.proc))
	end
	if KuAntennaStatus.status ~= animstate.CLOSED then
		oapi.writescenario_string(scn, "KUBAND", tostring(KuAntennaStatus.status-1).." "..tostring(KuAntennaStatus.proc))
	end
end

-- ==============================================================

function define_animations(vcidx)
	local switch_rot = _V(0,0,1)
	local switch_row1 = _V(1.3068,2.1991,12.7983)
	local switch_row2 = _V(1.2132,2.1377,12.7983)
	local switch_row3 = _V(1.1244,2.0794,12.7983)

	-- Animations for switches on panel R13L in the VC
	local VC_R13L_S1 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_1, switch_row1, switch_rot, 90.0*RAD)
	anim_VC_R13L[0] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[0], 0, 1, VC_R13L_S1)

	local VC_R13L_S2 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_2, switch_row1, switch_rot, 90.0*RAD)
	anim_VC_R13L[1] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[1], 0, 1, VC_R13L_S2)

	local VC_R13L_S3 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_3, switch_row1, switch_rot, 90.0*RAD)
	anim_VC_R13L[2] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[2], 0, 1, VC_R13L_S3)

	local VC_R13L_S4 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_4, switch_row1, switch_rot, 90.0*RAD)
	anim_VC_R13L[3] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[3], 0, 1, VC_R13L_S4)

	local VC_R13L_S5 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_5, switch_row2, switch_rot, 90.0*RAD)
	anim_VC_R13L[4] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[4], 0, 1, VC_R13L_S5)

	local VC_R13L_S6 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_6, switch_row2, switch_rot, 90.0*RAD)
	anim_VC_R13L[5] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[5], 0, 1, VC_R13L_S6)

	local VC_R13L_S7 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_7, switch_row2, switch_rot, 90.0*RAD)
	anim_VC_R13L[6] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[6], 0, 1, VC_R13L_S7)

	local VC_R13L_S8 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_8, switch_row2, switch_rot, 90.0*RAD)
	anim_VC_R13L[7] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[7], 0, 1, VC_R13L_S8)

	local VC_R13L_S9 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_9, switch_row2, switch_rot, 90.0*RAD)
	anim_VC_R13L[8] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[8], 0, 1, VC_R13L_S9)

	local VC_R13L_S10 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_10, switch_row3, switch_rot, 90.0*RAD)
	anim_VC_R13L[9] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[9], 0, 1, VC_R13L_S10)

	local VC_R13L_S11 = MGROUP_ROTATE(vcidx, GRP_VC.SwitchR13L_11, switch_row3, switch_rot, 90.0*RAD)
	anim_VC_R13L[10] = vi:create_animation(0.5)
	vi:add_animationcomponent(anim_VC_R13L[10], 0, 1, VC_R13L_S11)
end

-- ==============================================================

function register_VC()
	local tkbk_tex = oapi.get_texturehandle(sts.hOrbiterVCMesh, 5)

	-- register the complete panel for mouse events
	oapi.VC_register_area(AID_R13L, PANEL_REDRAW.NEVER, PANEL_MOUSE.LBDOWN)
	oapi.VC_set_areaclickmode_quadrilateral(AID_R13L, _V(1.3543,2.23023,12.8581), _V(1.3543,2.23023,12.5486), _V(1.0868,2.0547,12.8581), _V(1.0868,2.0547,12.5486))

	-- register the talkbacks
	oapi.VC_register_area(AID_R13L_TKBK1, _R(  0,0, 32,18), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, PANEL_MAP.NONE, tkbk_tex)
	oapi.VC_register_area(AID_R13L_TKBK2, _R( 32,0, 64,18), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, PANEL_MAP.NONE, tkbk_tex)
	oapi.VC_register_area(AID_R13L_TKBK3, _R( 64,0, 96,18), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, PANEL_MAP.NONE, tkbk_tex)
	oapi.VC_register_area(AID_R13L_TKBK4, _R( 96,0,128,18), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, PANEL_MAP.NONE, tkbk_tex)
	oapi.VC_register_area(AID_R13L_TKBK5, _R(128,0,160,18), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, PANEL_MAP.NONE, tkbk_tex)
	oapi.VC_register_area(AID_R13L_TKBK6, _R(160,0,192,18), PANEL_REDRAW.USER, PANEL_MOUSE.IGNORE, PANEL_MAP.NONE, tkbk_tex)

	tkbk_state = {-1,-1,-1,-1,-1,-1}
end

-- ==============================================================

function update_VC()
	vi:set_animation(anim_VC_R13L[0], BayDoor[1] == BD_ENABLE and 0 or 1)
	vi:set_animation(anim_VC_R13L[1], BayDoor[2] == BD_ENABLE and 0 or 1)

	vi:set_animation(anim_VC_R13L[2], MechPwr[1] == MP_ON and 0 or 1)
	vi:set_animation(anim_VC_R13L[3], MechPwr[2] == MP_ON and 0 or 1)

	vi:set_animation(anim_VC_R13L[4], BayDoorOp == BDO_OPEN and 0 or (BayDoorOp == BDO_CLOSE and 1 or 0.5))

	vi:set_animation(anim_VC_R13L[5], RadLatchCtrl[1] == LC_RELEASE and 0 or (RadLatchCtrl[1] == LC_LATCH and 1 or 0.5))
	vi:set_animation(anim_VC_R13L[6], RadLatchCtrl[2] == LC_RELEASE and 0 or (RadLatchCtrl[2] == LC_LATCH and 1 or 0.5))

	vi:set_animation(anim_VC_R13L[7], RadiatorCtrl[1] == RC_DEPLOY and 0 or (RadiatorCtrl[1] == RC_STOW and 1 or 0.5))
	vi:set_animation(anim_VC_R13L[8], RadiatorCtrl[2] == RC_DEPLOY and 0 or (RadiatorCtrl[2] == RC_STOW and 1 or 0.5))

	vi:set_animation(anim_VC_R13L[9], KuDirectCtrl == KU_DIRECT_ON and 0 or 1)
	vi:set_animation(anim_VC_R13L[10], KuCtrl == KU_DEPLOY and 0 or (KuCtrl == KU_STOW and 1 or 0.5))

	oapi.VC_trigger_redrawarea(-1, AID_R13L_TKBK1)
	oapi.VC_trigger_redrawarea(-1, AID_R13L_TKBK2)
	oapi.VC_trigger_redrawarea(-1, AID_R13L_TKBK3)
	oapi.VC_trigger_redrawarea(-1, AID_R13L_TKBK4)
	oapi.VC_trigger_redrawarea(-1, AID_R13L_TKBK5)
	oapi.VC_trigger_redrawarea(-1, AID_R13L_TKBK6)
end

-- ==============================================================

local tkbk_label = oapi.load_texture("Atlantis/tkbk_label.bmp")

function VC_draw_talkback(surf, idx, label)
	if tkbk_state[idx+1] == label then return false end -- nothing to do
	tkbk_state[idx+1] = label
	oapi.blt(surf, tkbk_label, 0, 0, label*32, 0, 32, 18)
	return true
end

-- ==============================================================

function VC_mouse_event(id, event, p)
	if id ~= AID_R13L then return false end
	local action = false

	if p.y >= 0.1113 and p.y <= 0.2461 then
		if p.x >= 0.1387 and p.x <= 0.2617  then
			BayDoor[1] = p.y < 0.1787 and BD_ENABLE or BD_DISABLE
			if BayDoor[1] == BD_DISABLE then set_door_action(animstate.STOPPED) end
			action = true
		elseif p.x >= 0.2910 and p.x <= 0.4180 then
			BayDoor[2] = p.y < 0.1787 and BD_ENABLE or BD_DISABLE
			if BayDoor[2] == BD_DISABLE then set_door_action(animstate.STOPPED) end
			action = true
		elseif p.x >= 0.4395 and p.x <= 0.5625 then
			MechPwr[1] = p.y < 0.1787 and MP_ON or MP_OFF
			if MechPwr[1] == MP_OFF then
				set_radiator_action(animstate.STOPPED)
				set_rad_latch_action(animstate.STOPPED)
			end
			action = true
		elseif p.x >= 0.5996 and p.x < 0.7188 then
			MechPwr[2] = p.y < 0.1787 and MP_ON or MP_OFF
			if MechPwr[2] == MP_OFF then
				set_radiator_action(animstate.STOPPED)
				set_rad_latch_action(animstate.STOPPED)
			end
			action = true
		end
	elseif p.y >= 0.4590 and p.y <= 0.6016 then
		local up = p.y < 0.5303
		if p.x >= 0.125 and p.x <= 0.2539 then
			BayDoorOp = up and (BayDoorOp == BDO_STOP and BDO_OPEN or BDO_STOP) or BayDoorOp == BDO_STOP and BDO_CLOSE or BDO_STOP
			set_door_action(BayDoorOp == BDO_OPEN and animstate.OPENING or BayDoorOp == BDO_CLOSE and animstate.CLOSING or animstate.STOPPED)
			action = true
		elseif p.x >= 0.2832 and p.x <= 0.4082 then
			RadLatchCtrl[1] = up and (RadLatchCtrl[1] == LC_OFF and LC_RELEASE or LC_OFF) or RadLatchCtrl[1] == LC_OFF and LC_LATCH or LC_OFF
			set_rad_latch_action(RadLatchCtrl[1] == LC_RELEASE and animstate.OPENING or RadLatchCtrl[1] == LC_LATCH and animstate.CLOSING or animstate.STOPPED)
			action = true
		elseif p.x >= 0.4414 and p.x <= 0.5645 then
			RadLatchCtrl[2] = up and (RadLatchCtrl[2] == LC_OFF and LC_RELEASE or LC_OFF) or RadLatchCtrl[2] == LC_OFF and LC_LATCH or LC_OFF
			set_rad_latch_action(RadLatchCtrl[2] == LC_RELEASE and animstate.OPENING or RadLatchCtrl[2] == LC_LATCH and animstate.CLOSING or animstate.STOPPED)
			action = true
		elseif p.x >= 0.5996 and p.x <= 0.7227 then
			RadiatorCtrl[1] = up and (RadiatorCtrl[1] == RC_OFF and RC_DEPLOY or RC_OFF) or RadiatorCtrl[1] == RC_OFF and RC_STOW or RC_OFF
			set_radiator_action(RadiatorCtrl[1] == RC_DEPLOY and animstate.OPENING or RadiatorCtrl[1] == RC_STOW and animstate.CLOSING or animstate.STOPPED)
			action = true
		elseif p.x >= 0.7559 and p.x <= 0.8789 then
			RadiatorCtrl[2] = up and (RadiatorCtrl[2] == RC_OFF and RC_DEPLOY or RC_OFF) or RadiatorCtrl[2] == RC_OFF and RC_STOW or RC_OFF
			set_radiator_action(RadiatorCtrl[2] == RC_DEPLOY and animstate.OPENING or RadiatorCtrl[2] == RC_STOW and animstate.CLOSING or animstate.STOPPED)
			action = true
		end
	elseif p.y >= 0.7891 and p.y <= 0.9219 then
		local up = p.y < 0.8555
		if p.x >= 0.1328 and p.x <= 0.2559 then
			KuDirectCtrl = up and KU_DIRECT_ON or KU_DIRECT_OFF
			if KuDirectCtrl == KU_DIRECT_ON then set_Ku_antenna_action(animstate.CLOSING) end
			action = true
		elseif p.x >= 0.2871 and p.x <= 0.4082 then
			KuCtrl = up and (KuCtrl == KU_GND and KU_DEPLOY or KU_GND) or KuCtrl == KU_GND and KU_STOW or KU_GND
			set_Ku_antenna_action(KuCtrl == KU_DEPLOY and animstate.OPENING or KuCtrl == KU_STOW and animstate.CLOSING or animstate.STOPPED)
			action = true
		end
	end

	if action then
		update_VC()
	end
	return false
end

-- ==============================================================

function VC_redraw_event(id, event, surf)
	if id == AID_R13L_TKBK1 then
		local label = {0,3,4,0,0}
		return VC_draw_talkback(surf, 0, label[BayDoorStatus.status + 1])
	end
	if id == AID_R13L_TKBK2 or id == AID_R13L_TKBK3 then
		local label = {0,2,4,0,0}
		return VC_draw_talkback(surf, id-AID_R13L_TKBK1, label[RadLatchStatus.status + 1])
	end
	if id == AID_R13L_TKBK4 or id == AID_R13L_TKBK5 then
		local label = {0,1,4,0,0}
		return VC_draw_talkback(surf, id-AID_R13L_TKBK1, label[RadiatorStatus.status + 1])
	end
	if id == AID_R13L_TKBK6 then
		local label = {0,1,4,0,0}
		return VC_draw_talkback(surf, id-AID_R13L_TKBK1, label[KuAntennaStatus.status + 1])
	end
	return false
end

local function get_baydoor_pos()
	return BayDoorStatus.proc
end
local function get_radiator_pos()
	return RadiatorStatus.proc
end
local function get_kuantenna_pos()
	return KuAntennaStatus.proc
end

-- return exported functions
return {
	init = init,
	define_animations = define_animations,
	get_baydoor_pos = get_baydoor_pos,
	get_radiator_pos = get_radiator_pos,
	get_kuantenna_pos = get_kuantenna_pos,
	parse_scenario_line = parse_scenario_line,
	save_state = save_state,
	step = step,
	set_door_action = set_door_action,
	set_Ku_antenna_action = set_Ku_antenna_action,
	register_VC = register_VC,
	update_VC = update_VC,
	VC_mouse_event = VC_mouse_event,
	VC_redraw_event = VC_redraw_event,
	revert_Ku_antenna_action = revert_Ku_antenna_action,
	revert_door_action = revert_door_action
}

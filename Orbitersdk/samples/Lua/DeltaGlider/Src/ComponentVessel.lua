-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local ComponentVessel = VesselClass()


function ComponentVessel:new()
	self.next_ssys_id = 0
	self.ssys = {}
end

--------------------------------------------------------------

function ComponentVessel:destroy()
	self.ssys = {}
end

--------------------------------------------------------------

function ComponentVessel:AddSubsystem(subsys)
	self.ssys[#self.ssys + 1] = subsys
	return subsys
end

--------------------------------------------------------------

function ComponentVessel:clbkSaveState(scn)
	-- Let subsystems write their parameters
	for _, v in ipairs(self.ssys) do
		v:clbkSaveState(scn)
	end
end

--------------------------------------------------------------

function ComponentVessel:clbkParseScenarioLine(line)
	for _, v in ipairs(self.ssys) do
		if v:clbkParseScenarioLine(line) then
			return true
		end
	end
	return false
end

--------------------------------------------------------------

function ComponentVessel:clbkPostCreation()
	for _, v in ipairs(self.ssys) do
		v:clbkPostCreation()
	end
end

--------------------------------------------------------------

function ComponentVessel:clbkDrawHUD(mode, hps, skp)
	for _, v in ipairs(self.ssys) do
		v:clbkDrawHUD(mode, hps, skp)
	end

	return true
end

--------------------------------------------------------------

function ComponentVessel:clbkRenderHUD(mode, hps, hTex)
	for _, v in ipairs(self.ssys) do
		v:clbkRenderHUD(mode, hps, hTex)
	end
end

--------------------------------------------------------------

function ComponentVessel:clbkPlaybackEvent(simt, event_t, event_type, event)
	for _, v in ipairs(self.ssys) do
		if v:clbkPlaybackEvent(simt, event_t, event_type, event) then
			return true
		end
	end
	return false
end

--------------------------------------------------------------

function ComponentVessel:clbkPreStep(simt, simdt, mjd)
	for _, v in ipairs(self.ssys) do
		v:clbkPreStep(simt, simdt, mjd)
	end
end

--------------------------------------------------------------

function ComponentVessel:clbkPostStep(simt, simdt, mjd)
	for _, v in ipairs(self.ssys) do
		v:clbkPostStep(simt, simdt, mjd)
	end
end

--------------------------------------------------------------

function ComponentVessel:clbkReset2D(panelid, hMesh)
	for _, v in ipairs(self.ssys) do
		v:clbkReset2D(panelid, hMesh)
	end
end

--------------------------------------------------------------

function ComponentVessel:clbkResetVC(vcid, hMesh)
	for _, v in ipairs(self.ssys) do
		v:clbkResetVC(vcid, hMesh)
	end
end

--------------------------------------------------------------

function ComponentVessel:clbkLoadPanel2D(panelid, hPanel, viewW, viewH)
	local b = false
	for _, v in ipairs(self.ssys) do
		local bi = v:clbkLoadPanel2D(panelid, hPanel, viewW, viewH)
		b = b or bi
	end
	return b
end

--------------------------------------------------------------

function ComponentVessel:clbkLoadVC(vcid)
	local b = false
	for _, v in ipairs(self.ssys) do
		local bi = v:clbkLoadVC(vcid)
		b = b or bi
	end
	return b
end

--------------------------------------------------------------

function ComponentVessel:clbkVCMouseEvent(elid, event, p)
	local subsys = math.floor(elid/1000)
--	oapi.dbg_out(string.format("clbkVCMouseEvent elid=%d ssys=%d", elid, subsys))
	local el = self.ssys[subsys]
	if el then
		return el:clbkVCMouseEvent(elid, event, p)
	end
	return false
end

--------------------------------------------------------------

function ComponentVessel:clbkVCRedrawEvent(elid, event, hMesh, hSurf)
	local subsys = math.floor(elid/1000)
	local el = self.ssys[subsys]
	if el then
		return el:clbkVCRedrawEvent(elid, event, hMesh, hSurf)
	end
	return false
end

--------------------------------------------------------------

function ComponentVessel:clbkConsumeBufferedKey(key, down, kstate)
	for _, v in ipairs(self.ssys) do
		local res = v:clbkConsumeBufferedKey(key, down, kstate)
		if res then
			return res
		end
	end
	return false
end

--------------------------------------------------------------

function ComponentVessel:clbkConsumeDirectKey(kstate)
	for _, v in ipairs(self.ssys) do
		local res = v:clbkConsumeDirectKey(kstate)
		if res then
			return true
		end
	end
	return false
end

function ComponentVessel:GetSsysId()
	local id = self.next_ssys_id
	self.next_ssys_id = self.next_ssys_id + 1
	return id
end

return ComponentVessel

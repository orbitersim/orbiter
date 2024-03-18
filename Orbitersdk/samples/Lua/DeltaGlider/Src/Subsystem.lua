-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: DeltaGlider.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local Subsystem = Class()

function Subsystem:new(param)
	-- param can be either a ComponentVessel or a Subsystem
	-- We check for the presence of the ssys field which
	-- exists only on ComponentVessel instances
	if param.ssys then
		self.vessel = param
		self.parent = nil              -- top-level subsystem
		self.id = param:GetSsysId()  -- assign a top-level subsystem id
	else
		self.parent = param
		self.vessel = param.vessel
		self.id = param.id           -- inherit the parent id
	end
	self.child = {}                  --list of child systems
	self.element = {}                --list of panel elements
end

function Subsystem:destroy()
end

function Subsystem:AddSubsystem(subsys)
	self.child[#self.child + 1] = subsys
	return subsys
end

function Subsystem:Parent()
	return self.parent
end

function Subsystem:Vessel()
	return self.vessel
end

function Subsystem:Id()
	return self.id
end

function Subsystem:AddElement(el)
	-- panel elements are always managed by the top-level subsystem
	if self.parent then
		return self.parent:AddElement(el), el
	else
		self.element[#self.element + 1] = el
		return #self.element - 1 + (self.id + 1) * 1000, el --create unique id
	end
end

function Subsystem:clbkPostCreation()
	for _, v in pairs(self.child) do
		v:clbkPostCreation()
	end
end

function Subsystem:clbkSaveState(scn)
	for _, v in pairs(self.child) do
		v:clbkSaveState(scn)
	end
end

function Subsystem:clbkParseScenarioLine(line)
	for _, v in pairs(self.child) do
		if v:clbkParseScenarioLine(line) then
			return true
		end
	end
	return false
end

function Subsystem:clbkPreStep(simt, simdt, mfd)
	for _, v in pairs(self.child) do
		v:clbkPreStep(simt, simdt, mfd)
	end
end

function Subsystem:clbkPostStep(simt, simdt, mfd)
	for _, v in pairs(self.child) do
		v:clbkPostStep(simt, simdt, mfd)
	end
end

function Subsystem:clbkLoadPanel2D(panelid, hPanel, viewW, viewH)
	for _, v in pairs(self.element) do
		v:LoadPanel2D(panelid, hPanel, viewW, viewH)
	end

	local b = false
	for _, v in pairs(self.child) do
		local bi = v:clbkLoadPanel2D(panelid, hPanel, viewW, viewH)
		b = b or bi
	end
	return b
end

function Subsystem:clbkLoadVC(vcid)
	for _, v in pairs(self.element) do
		v:LoadVC(vcid)
	end

	local b = false
	for _, v in pairs(self.child) do
		local bi = v:clbkLoadVC(vcid)
		b = b or bi
	end
	return b
end

function Subsystem:clbkReset2D(panelid, hMesh)
	for _, v in pairs(self.child) do
		v:clbkReset2D(panelid, hMesh)
	end

	for _, v in pairs(self.element) do
		v:Reset2D(panelid, hMesh)
	end
end

function Subsystem:clbkResetVC(vcid, hMesh)
	for _, v in pairs(self.child) do
		v:clbkResetVC(vcid, hMesh)
	end

	for _, v in pairs(self.element) do
		v:ResetVC(hMesh)
	end
end

function Subsystem:clbkVCRedrawEvent(elid, event, hMesh, hSurf)
	-- note: this callback is not distributed to children, because the top-level
	-- subsystem manages all panel elements

	elid = elid - (self.id+1)*1000 +1-- convert to index
	local el = self.element[elid]
	if el then
		return el:RedrawVC(hMesh, hSurf)
	end
	return false
end

function Subsystem:clbkVCMouseEvent(elid, event, p)
	-- note: this callback is not distributed to children, because the top-level
	-- subsystem manages all panel elements
--	oapi.write_log(string.format("clbkVCMouseEvent id:%d elid:%d -> %d", self.id, elid, elid - (self.id+1)*1000))
	elid = elid - (self.id+1)*1000 +1-- convert to index
	local el = self.element[elid]
	if el then
		return el:ProcessMouseVC (event, p)
	end
	return false
end

function Subsystem:clbkDrawHUD(mode, hps, skp)
	local b = false
	for _, v in pairs(self.child) do
		local bi = v:clbkDrawHUD(mode, hps, skp)
		b = b or bi
	end
	return b
end

function Subsystem:clbkRenderHUD(mode, hps, hTex)
	for _, v in pairs(self.child) do
		v:clbkRenderHUD(mode, hps, hTex)
	end
end

function Subsystem:clbkPlaybackEvent(simt, event_t, event_type, event)
	for _, v in pairs(self.child) do
		if v:clbkPlaybackEvent(simt, event_t, event_type, event) then
			return true
		end
	end
	return false
end

function Subsystem:clbkConsumeBufferedKey(key, down, kstate)
	for _, v in pairs(self.child) do
		local res = v:clbkConsumeBufferedKey(key, down, kstate)
		if res ~= 0 then
			return res
		end
	end
	return 0
end

function Subsystem:clbkConsumeDirectKey(kstate)
	for _, v in pairs(self.child) do
		local res = v:clbkConsumeDirectKey(kstate)
		if res ~= 0 then
			return res
		end
	end
	return 0
end


return Subsystem

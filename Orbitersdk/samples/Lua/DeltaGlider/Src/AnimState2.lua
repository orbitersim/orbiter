-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- ==============================================================
--               ORBITER MODULE: AnimState2.lua
--                  Part of the ORBITER SDK
--
-- Original Delta glider rewritten in lua
-- ==============================================================

local AnimState2 = Class()

--==============================================================

function AnimState2:new(operating_speed, initial_state)
	self.state = initial_state or 0.0
	self.speed = 0.0
	self.inc_speed = operating_speed or 1.0
	self.dec_speed = operating_speed and -operating_speed or -1.0
	self.drag_element = nil
end

--------------------------------------------------------------

function AnimState2:SetDragElement(drag_element)
	self.drag_element =  drag_element
end

function AnimState2:UpdateDragElement(drag_element)
	if self.drag_element then
		self.drag_element:set(self.state)
	end
end
--------------------------------------------------------------

function AnimState2:SetOperatingSpeed(opspeed)
	self.inc_speed =  opspeed
	self.dec_speed = -opspeed
end

--------------------------------------------------------------

function AnimState2:SetState(_state, _speed)
	self.state = _state
	self.speed = _speed
	if self.state >= 1.0 then
		self.state = 1.0
		if self.speed > 0.0 then
			self.speed = 0.0
		end
	elseif self.state <= 0.0 then
		self.state = 0.0
		if self.speed < 0.0 then
			self.speed = 0.0
		end
	end
	self:UpdateDragElement()
end

--------------------------------------------------------------

function AnimState2:Open()
	if self.state < 1.0 then
		self.speed = self.inc_speed
	end
end

--------------------------------------------------------------

function AnimState2:Close()
	if self.state > 0.0 then
		self.speed = self.dec_speed
	end
end

--------------------------------------------------------------

function AnimState2:Stop()
	self.speed = 0.0
end

--------------------------------------------------------------

function AnimState2:SetOpened()
	self.state = 1.0
	self.speed = 0.0
	self:UpdateDragElement()
end

--------------------------------------------------------------

function AnimState2:SetClosed()
	self.state = 0.0
	self.speed = 0.0
	self:UpdateDragElement()
end

--------------------------------------------------------------

function AnimState2:Process(dt)
	if self.speed ~= 0 then
		self.state = self.state + self.speed * dt
		if self.state <= 0.0 then
			self.state = 0.0
			self.speed = 0.0
		elseif self.state >= 1.0 then
			self.state = 1.0
			self.speed = 0.0
		end
		self:UpdateDragElement()
		return true
	else
		return false
	end
end

--------------------------------------------------------------

function AnimState2:SaveState(scn, label)
	if self.state ~= 0 then
		oapi.writescenario_string(scn, label, string.format("%0.4f %0.4f", self.state, self.speed))
	end
end

--------------------------------------------------------------

function AnimState2:ParseScenarioLine(line, label)
	local match = {}
	if scenario_line_match(line, label.." %f %f", match) then
		self.state = match.res[1]
		self.speed = match.res[2]
		self:UpdateDragElement()
		return true
	end
	return false
end

--==============================================================

function AnimState2:IsActive()  return self.speed ~= 0.0 end
function AnimState2:IsOpen()    return self.state == 1.0 end
function AnimState2:IsClosed()  return self.state == 0.0 end
function AnimState2:IsOpening() return self.speed >  0.0 end
function AnimState2:IsClosing() return self.speed <  0.0 end
function AnimState2:State()     return self.state end
function AnimState2:Speed()     return self.speed end

--	inline const double *StatePtr() const { return &state; }

return AnimState2

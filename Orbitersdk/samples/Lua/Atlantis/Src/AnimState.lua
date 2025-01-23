-- Copyright (c) Martin Schweiger
-- Copyright 2024 (c) Gondos
-- Licensed under the MIT License

-- Converted from VesselAPI.h

local animstate = Class()

animstate.STOPPED = 0
animstate.CLOSED  = 1
animstate.OPEN    = 2
animstate.CLOSING = 3
animstate.OPENING = 4

function animstate:new(proc, speed, status)
	self.status = status or animstate.STOPPED
	self.proc   = proc
	self.speed  = speed
end

function animstate:moving()
	return self.status >= animstate.CLOSING
end

function animstate:move(simdt)
	if self.status >= animstate.CLOSING then
		local da = simdt * self.speed
		if self.status == animstate.CLOSING then -- retract
			if self.proc > 0.0 then
				self.proc = math.max(0.0, self.proc - da)
			else                 
				self.status = animstate.CLOSED
			end
		else                                     -- deploy
			if self.proc < 1.0 then
				self.proc = math.min(1.0, self.proc + da)
			else
				self.status = animstate.OPEN
			end
		end
		return true
	else
		return false
	end

end
--[[
class AnimState {
public:
	void Set (Action a, double p) { action = a, pos = p; }
	bool Move (double dp) {
		if (!Moving()) return false;
		if (Closing()) {
			if ((pos = (std::max)(0.0, pos - dp)) == 0.0) action = CLOSED;
		} else {
			if ((pos = (std::min)(1.0, pos + dp)) == 1.0) action = OPEN;
		}
		return true;
	}
	bool Static() const { return action < CLOSING; }
	bool Stopped() const { return action == STOPPED; }
	bool Closed() const { return action == CLOSED; }
	bool Open() const { return action == OPEN; }
	bool Closing() const { return action == CLOSING; }
	bool Opening() const { return action == OPENING; }
	friend OAPIFUNC void WriteScenario_state (FILEHANDLE f, char *tag, const AnimState &s);
	friend OAPIFUNC void sscan_state (char *str, AnimState &s);
};
]]
return animstate

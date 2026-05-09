--- @sync entry

local DEFAULT = { 1, 3, 4 }
local WIDE = { 1, 1, 6 }

local function eq(r, t)
	return r.parent == t[1] and r.current == t[2] and r.preview == t[3]
end

local function entry()
	rt.mgr.ratio = eq(rt.mgr.ratio, WIDE) and DEFAULT or WIDE
	ya.emit("app:resize", {})
end

return { entry = entry }

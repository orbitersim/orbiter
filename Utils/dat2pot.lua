-- dat2pot.lua
-- Usage:
--   lua dat2pot.lua [--strip-prefix=PATH] input/system.dat output/system.pot

local strip_prefix = nil
local positional = {}

for i = 1, #arg do
    local a = arg[i]

    if not strip_prefix then
        local p = a:match("^%-%-strip%-prefix=(.+)")
        if p then
            p = p:gsub("\\", "/")
            if p:sub(-1) ~= "/" then
                p = p .. "/"
            end
            strip_prefix = p
        else
            table.insert(positional, a)
        end
    else
        table.insert(positional, a)
    end
end

local input_path  = positional[1]
local output_path = positional[2]

if not input_path or not output_path then
    io.stderr:write("Usage: lua dat2pot.lua [--strip-prefix=PATH] <input_dat> <output_pot>\n")
    os.exit(1)
end

local function normalize_path(path)
    path = path:gsub("\\", "/")
    path = path:gsub("/+", "/")
    path = path:gsub("^%./", "")
    return path
end

local function make_relative(path)
    path = normalize_path(path)

    if strip_prefix and path:sub(1, #strip_prefix) == strip_prefix then
        local rel = path:sub(#strip_prefix + 1)
        return rel ~= "" and rel or "."
    end

    return path
end

------------------------------------------------------------
-- Extract directory name for msgctxt
------------------------------------------------------------
local function get_directory_name(path)
    path = path:gsub("\\", "/")
    local dir = path:match("(.+)/[^/]+$")
    if not dir then return "" end
    local name = dir:match(".+/([^/]+)$")
    return name or ""
end

local msgctxt = get_directory_name(input_path)
msgctxt="Flight"
------------------------------------------------------------
-- Escape string for PO format
------------------------------------------------------------
local function po_escape(str)
    str = str:gsub("\\", "\\\\")
    str = str:gsub("\"", "\\\"")
    str = str:gsub("\t", "\\t")
    str = str:gsub("\n", "\\n")
    return str
end

------------------------------------------------------------
-- Read system.dat
------------------------------------------------------------
local infile = io.open(input_path, "r")
if not infile then
    io.stderr:write("Cannot open input file: " .. input_path .. "\n")
    os.exit(1)
end

local messages = {}    -- msgid -> {refs}
local msg_order = {}   -- msgid in order of first appearance
local line_number = 0

for line in infile:lines() do
    line_number = line_number + 1

    local text = line:match("^%s*%d+%.?%d*%s+NOTE%s+(.+)$")
    if text then
        local msgid = po_escape(text)

        if not messages[msgid] then
            messages[msgid] = {}
            table.insert(msg_order, msgid)  -- preserve order
        end

        local rel = make_relative(input_path)
        table.insert(messages[msgid], rel .. ":" .. line_number)
    end
end

infile:close()

if next(messages) == nil then
    -- Nothing to extract
    os.exit(0)
end

------------------------------------------------------------
-- Write POT
------------------------------------------------------------
local outfile = io.open(output_path, "wb")
if not outfile then
	io.stderr:write("Cannot open output file: " .. output_path .. "\n")
	os.exit(1)
end

-- Header
outfile:write('msgid ""\n')
outfile:write('msgstr ""\n')
outfile:write('"Project-Id-Version: Orbiter Flights\\n"\n')
outfile:write('"Content-Type: text/plain; charset=UTF-8\\n"\n')
outfile:write('"Plural-Forms: nplurals=2; plural=(n != 1);\\n"\n')
outfile:write('"Content-Transfer-Encoding: 8bit\\n"\n\n')

-- Entries in original order
for _, msgid in ipairs(msg_order) do
	local refs = messages[msgid]
	outfile:write("#: " .. table.concat(refs, " ") .. "\n")

	if msgctxt ~= "" then
		outfile:write('msgctxt "' .. po_escape(msgctxt) .. '"\n')
	end

	outfile:write('msgid "' .. msgid .. '"\n')
	outfile:write('msgstr ""\n\n')
end

outfile:close()

-- print("Generated: " .. output_path)

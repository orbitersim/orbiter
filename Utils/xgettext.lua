-- xgettext.lua
-- Usage:
--   lua xgettext.lua output.pot filelist.txt

local output_file = arg[1]
local list_file   = arg[2]

if not output_file or not list_file then
    print("Usage: lua xgettext.lua output.pot filelist.txt")
    os.exit(1)
end

-----------------------------------------------------------------------
-- Read file list
-----------------------------------------------------------------------
local sources = {}
for line in io.lines(list_file) do
    if line:match("%S") then
        table.insert(sources, line)
    end
end

local pot_entries = {}

-----------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------
local function escape_po_string(s)
    return (s
        :gsub("\\", "\\\\")
        :gsub("\"", "\\\"")
        :gsub("\n", "\\n"))
end

local function is_ident_char(c)
    return c and c:match("[%w_]")
end

local function add_entry(msgid, msgid_plural, msgctxt, comment, ref)
    local key = (msgctxt or "") .. "\x04" .. msgid .. "\x04" .. (msgid_plural or "")
    local e = pot_entries[key]
    if not e then
        e = {
            msgid = msgid,
            msgid_plural = msgid_plural,
            msgctxt = msgctxt,
            comment = comment,
            refs = {}
        }
        pot_entries[key] = e
    end
    if ref then e.refs[ref] = true end
    if comment and not e.comment then e.comment = comment end
end

-----------------------------------------------------------------------
-- Collect a macro call across multiple lines
-----------------------------------------------------------------------
local function collect_call(lines, start_line, start_pos)
    local buf = {}
    local depth = 0
    local line_no = start_line
    local pos = start_pos

    local in_string = nil   -- "'" or '"'
    local escape = false

    while line_no <= #lines do
        local line = lines[line_no]
        local chunk = line:sub(pos)
        table.insert(buf, chunk)

        local i = 1
        while i <= #chunk do
            local ch = chunk:sub(i, i)
            local nextch = chunk:sub(i + 1, i + 1)

            -- Handle escaping inside strings
            if in_string then
                if escape then
                    escape = false
                elseif ch == "\\" then
                    escape = true
                elseif ch == in_string then
                    in_string = nil
                end

            else
                -- Line comment (// ...)
                if ch == "/" and nextch == "/" then
                    break

                -- Enter string
                elseif ch == "'" or ch == '"' then
                    in_string = ch

                -- Parentheses tracking
                elseif ch == "(" then
                    depth = depth + 1
                elseif ch == ")" then
                    depth = depth - 1
                    if depth == 0 then
                        return table.concat(buf, "\n")
                    end
                end
            end

            i = i + 1
        end

        line_no = line_no + 1
        pos = 1
    end

    return nil
end
-----------------------------------------------------------------------
-- Parse arguments and concatenate adjacent string literals
-----------------------------------------------------------------------
local function parse_string_arguments(argstr)
    local args = {}
    local i = 1
    local len = #argstr

    local function skip_ws()
        while i <= len and argstr:sub(i,i):match("%s") do
            i = i + 1
        end
    end

    while i <= len do
        skip_ws()

        if argstr:sub(i,i) ~= '"' then
            while i <= len and argstr:sub(i,i) ~= ',' do
                i = i + 1
            end
            i = i + 1
        else
            local parts = {}

            while argstr:sub(i,i) == '"' do
                i = i + 1
                local buf = {}

                while i <= len do
                    local c = argstr:sub(i,i)
                    if c == "\\" then
                        local n = argstr:sub(i+1,i+1)
                        if n == "n" then table.insert(buf, "\n")
                        elseif n == "t" then table.insert(buf, "\t")
                        elseif n == "r" then table.insert(buf, "\r")
                        else table.insert(buf, n) end
                        i = i + 2
                    elseif c == '"' then
                        i = i + 1
                        break
                    else
                        table.insert(buf, c)
                        i = i + 1
                    end
                end

                table.insert(parts, table.concat(buf))
                skip_ws()
            end

            table.insert(args, table.concat(parts))

            while i <= len and argstr:sub(i,i) ~= ',' do
                i = i + 1
            end
            i = i + 1
        end
    end

    return args
end

-----------------------------------------------------------------------
-- Scan one file
-----------------------------------------------------------------------
local function scan_file(filename)
    local f = io.open(filename, "r")
    if not f then return end

    local lines = {}
    for l in f:lines() do table.insert(lines, l) end
    f:close()

    local pending_comment = nil

    -- Longest names first
    local patterns = {
        { name="_abbr2", abbr=true, length=2  },
        { name="_abbr3", abbr=true, length=3  },
        { name="_abbr4", abbr=true, length=4  },
        { name="_nc", ctx=true,  plural=true  },
        { name="_c",  ctx=true,  plural=false },
        { name="_n",  ctx=false, plural=true  },
        { name="_",   ctx=false, plural=false },
    }
	local file_context = nil
    for lineno, line in ipairs(lines) do
        local c = line:match("^%s*//%s*TRANSLATORS:%s*(.+)")
        if c then 
			pending_comment = c
		end    
		
		local ctx = line:match('#define%s+TRANSLATION_CONTEXT%s+"([^"]+)"')
		if ctx then
			file_context = ctx
		end

		local comment_consumed = false
        for _, p in ipairs(patterns) do
            local search = 1
            while true do
                local s, e = line:find(p.name .. "%s*%(", search)
                if not s then break end

                local prev = s > 1 and line:sub(s-1, s-1) or nil
                if not is_ident_char(prev) then
                    local call_text = collect_call(lines, lineno, e)
                    if call_text then
                        local args = call_text:match("%((.*)%)")
                        if args then
                            local strs = parse_string_arguments(args)
                            local ref = filename .. ":" .. lineno

							if p.abbr and #strs >= 1 then
								-- Warning: must match the context used in i18n.h
								add_entry(strs[1], nil, "Abbreviation - "..p.length.." letters", pending_comment, ref)
                                comment_consumed = true
                            elseif p.ctx and p.plural and #strs >= 3 then
                                add_entry(strs[2], strs[3], strs[1], pending_comment, ref)
                                comment_consumed = true
                            elseif p.ctx and not p.plural and #strs >= 2 then
                                add_entry(strs[2], nil, strs[1], pending_comment, ref)
                                comment_consumed = true
                            elseif not p.ctx and p.plural and #strs >= 2 then
                                add_entry(strs[1], strs[2], file_context, pending_comment, ref)
                                comment_consumed = true
							elseif not p.ctx and #strs >= 1 then
								add_entry(strs[1], nil, file_context, pending_comment, ref)
                                comment_consumed = true
                            end
                        end
                    end
                end

                search = e + 1
            end
        end
		if comment_consumed then
			pending_comment = nil
		end
    end
end

-----------------------------------------------------------------------
-- Scan all sources
-----------------------------------------------------------------------
for _, f in ipairs(sources) do
    scan_file(f)
end

-----------------------------------------------------------------------
-- Deterministic sort
-----------------------------------------------------------------------
local entries = {}
for _, e in pairs(pot_entries) do
    local first_ref
    for r in pairs(e.refs) do
        if not first_ref or r < first_ref then
            first_ref = r
        end
    end
    if first_ref then
        local file, line = first_ref:match("^(.*):(%d+)$")
        table.insert(entries, {
            entry = e,
            file = file or "",
            line = tonumber(line) or 0,
            ctx = e.msgctxt or ""
        })
    end
end

table.sort(entries, function(a, b)
    if a.file ~= b.file then return a.file < b.file end
    if a.ctx ~= b.ctx then return a.ctx < b.ctx end
    return a.line < b.line
end)

-----------------------------------------------------------------------
-- Write .pot file
-----------------------------------------------------------------------
local f = io.open(output_file, "w")
print("Writing .pot file to " .. output_file)

f:write(table.concat({
    "# This file was auto-generated, DO NOT modify it directly",
    "msgid \"\"",
    "msgstr \"\"",
    "\"Content-Type: text/plain; charset=UTF-8\\n\"",
    "\"Plural-Forms: nplurals=2; plural=(n != 1);\\n\"",
    "\"X-Generator: Lua xgettext.lua\\n\"",
    ""
}, "\n"), "\n")

for _, item in ipairs(entries) do
    local e = item.entry

    local refs = {}
    for r in pairs(e.refs) do table.insert(refs, r) end
    table.sort(refs)
    if #refs > 0 then
        f:write("#: ", table.concat(refs, " "), "\n")
    end

    if e.comment then
        f:write("#. ", e.comment, "\n")
    end

    if e.msgctxt then
        f:write('msgctxt "', escape_po_string(e.msgctxt), '"\n')
    end

    if e.msgid_plural then
        f:write('msgid "', escape_po_string(e.msgid), '"\n')
        f:write('msgid_plural "', escape_po_string(e.msgid_plural), '"\n')
        f:write('msgstr[0] ""\n')
        f:write('msgstr[1] ""\n\n')
    else
        f:write('msgid "', escape_po_string(e.msgid), '"\n')
        f:write('msgstr ""\n\n')
    end
end

f:close()
print("Wrote .pot file to " .. output_file)

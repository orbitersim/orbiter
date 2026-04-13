-- Copyright (c) 2026 Gondos
-- Licensed under the MIT License

-- Argument extraction
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

local input_file  = positional[1]
local output_file = positional[2]

if not output_file or not input_file then
    print("Usage: lua report_i18n.lua [--strip-prefix=PATH] file_list.txt report.md")
    os.exit(1)
end

local function make_relative(path)
    if strip_prefix and path:sub(1, #strip_prefix) == strip_prefix then
        local rel = path:sub(#strip_prefix + 1)
        return rel ~= "" and rel or "."
    end

    return path
end

local function parse_po_file(path)
    local results = {}

    local file = io.open(path, "r")
    if not file then
        error("Could not open file: " .. path)
    end

    local current_msgctxt
    local current_msgid
    local current_msgstr
    local state

    for line in file:lines() do
        line = line:gsub("\r", ""):match("^%s*(.-)%s*$")
        if line ~= "" and line:sub(1,1) ~= "#" then
            -- msgctxt
            local v = line:match('^msgctxt%s+"(.*)"$')
            if v then
                current_msgctxt = v
                state = "msgctxt"
            else
                -- msgid
                v = line:match('^msgid%s+"(.*)"$')
                if v then
                    current_msgid = v
                    state = "msgid"

                else
                    -- msgstr
                    v = line:match('^msgstr%s+"(.*)"$')
                    if v then
                        current_msgstr = v
                        state = "msgstr"
                    else
                        -- continuation lines
                        local cont = line:match('^"(.*)"$')
                        if cont then
                            if state == "msgid" then
                                current_msgid = (current_msgid or "") .. cont
                            elseif state == "msgstr" then
                                current_msgstr = (current_msgstr or "") .. cont
                            elseif state == "msgctxt" then
                                current_msgctxt = (current_msgctxt or "") .. cont
                            end
                        end
                    end
                end
            end
        else
            -- flush entry on empty or comment line
            local key
            if current_msgctxt then
                key = current_msgctxt .. "\004" .. current_msgid
            else
                key = current_msgid
            end
            if key and key~="" then
                results[key]=current_msgstr
            end
            current_msgctxt = nil
            current_msgid = nil
            current_msgstr = nil
            state = nil
        end
    end

    if current_msgid ~= "" then
        local key
        if current_msgctxt then
            key = current_msgctxt .. "\004" .. current_msgid
        else
            key = current_msgid
        end
        if key and key ~= "" then
            results[key]=current_msgstr
        end
    end

    file:close()
    return results
end

local pots = {}
local pos = {}
local locales = {}

local function normalize(path)
    return path:gsub("\\", "/")
end

local function dir_file(path)
    local dir, file = path:match("^(.*)/([^/]+)$")
    if not dir then return end

    return dir, file
end

local function base_ext(file)
    local base, ext = file:match("(.+)%.([%w_]+)$")
    if not base then return end

    return base, ext
end

-- Parse input_file
-- For each line, extract the content of the .po/.pot file
for line in io.lines(input_file) do
    line = line:match("^%s*(.-)%s*$")
    if line ~= "" then
        local path = normalize(line)
        local dir, file = dir_file(path)

        if dir and file then
            local _, ext = base_ext(file)

            if ext == "pot" then
                local base = path:match("^(.*)%.pot$")
                local entry = {}
                entry.path = path
                entry.content = parse_po_file(path)
                pots[base] = entry
            elseif ext == "po" then
                local base, locale = path:match("^(.*)%.([%a_%-]+)%.po$")
                if locale then
                    locales[locale] = locales[locale] or {}
                    local entry = {}
                    entry.path = path
                    entry.content = parse_po_file(path)
                    locales[locale][base] = entry
                end
            end
        end
    end
end

function tableLength(tab)
  local length = 0
  for _ in pairs(tab) do
    length = length + 1
  end
  return length
end

-- Create an iterator to iterate over a map in a sorted way
function orderedMap(t)
    local keys = {}

    for k in pairs(t) do
        table.insert(keys, k)
    end

    table.sort(keys, function(a, b)
        -- if sorting locales, sort by key
        if t[a].path == nil or t[b].path == nil then
            return a<b
        end
        -- else we're sorting pot/po file list
        return t[a].path < t[b].path
    end)

    local i = 0

    return function()
        i = i + 1
        local key = keys[i]
        if key ~= nil then
            return key, t[key]
        end
    end
end

function coverage(content_pot, content_po)
    local total = 0
    local translated = 0
    for k, v in pairs(content_pot) do
        total = total + 1
        if content_po[k] ~= "" and content_po[k] ~= nil  then
            translated = translated + 1
        end
    end

    if total == translated then
        return "✅ OK"
    else
        return "⚠️ ("..translated.."/"..total..")"
    end
end

function url_encode(str)
    if str then
        str = str:gsub("\n", "\r\n")
        str = str:gsub("([^%w%-_%.~/])", function(c)
            return string.format("%%%02X", string.byte(c))
        end)
    end
    return str
end

local outfile = io.open(output_file, "wb")
if not outfile then
	io.stderr:write("Cannot open output file: " .. output_file .. "\n")
	os.exit(1)
end

outfile:write("# List of POT files\n")
outfile:write("<details>\n")
outfile:write("<summary>Show files</summary>\n\n")
outfile:write("| File | Entries |\n")
outfile:write("|------|---------|\n")
for pot, pot_path in orderedMap(pots) do
    local rel = make_relative(pot_path.path)
    outfile:write("| ["..rel.."]("..url_encode(rel)..") | "..tableLength(pot_path.content).." |\n")
end
outfile:write("</details>\n\n")

outfile:write("# Translation status for detected locales\n")
for locale, po_files in orderedMap(locales) do
    outfile:write("<details>\n")
    outfile:write("<summary>"..locale.."</summary>\n\n")
    outfile:write("| File | Status |\n")
    outfile:write("|------|--------|\n")
    for pot, pot_path in orderedMap(pots) do
        if po_files[pot] then
            local cov = coverage(pot_path.content, po_files[pot].content)
            outfile:write("| "..make_relative(po_files[pot].path).." | "..cov.." |\n")
        else
            outfile:write("| "..make_relative(pot_path.path:gsub(".pot", "."..locale..".po")).." | ❌ PO file not found |\n")
        end
    end
    outfile:write("</details>\n\n")
end

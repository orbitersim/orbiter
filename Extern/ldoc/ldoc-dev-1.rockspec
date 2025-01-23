local package_name = "ldoc"
local package_version = "dev"
local rockspec_revision = "1"
local github_account_name = "lunarmodules"

rockspec_format = "3.0"
package = package_name
version = package_version .. "-" .. rockspec_revision

source = {
  url = "git+https://github.com/" .. github_account_name .. "/" .. package_name .. ".git"
}

if package_version == "dev" then
  source.branch = "master"
else
  source.tag = "v" .. package_version
end

description = {
  summary = "A Lua Documentation Tool",
  detailed = [[
      LDoc is a LuaDoc-compatible documentation generator which can also
      process C extension source. Markdown may be optionally used to
      render comments, as well as integrated readme documentation and
      pretty-printed example files
    ]],
  homepage="http://lunarmodules.github.io/ldoc",
  issues_url = "https://github.com/lunarmodules/ldoc/issues",
  maintainer="caleb@alerque.com",
  license = 'MIT <http://opensource.org/licenses/MIT>'
}

dependencies = {
  "markdown",
  "penlight",
}

build = {
  type = "builtin",
  modules = {
    ["ldoc.tools"] = "ldoc/tools.lua",
    ["ldoc.lang"] = "ldoc/lang.lua",
    ["ldoc.parse"] = "ldoc/parse.lua",
    ["ldoc.html"] = "ldoc/html.lua",
    ["ldoc.lexer"] = "ldoc/lexer.lua",
    ["ldoc.markup"] = "ldoc/markup.lua",
    ["ldoc.prettify"] = "ldoc/prettify.lua",
    ["ldoc.markdown"] = "ldoc/markdown.lua",
    ["ldoc.doc"] = "ldoc/doc.lua",
    ["ldoc.html.ldoc_ltp"] = "ldoc/html/ldoc_ltp.lua",
    ["ldoc.html.ldoc_md_ltp"] = "ldoc/html/ldoc_md_ltp.lua",
    ["ldoc.html.ldoc_css"] = "ldoc/html/ldoc_css.lua",
    ["ldoc.html._code_css"] = "ldoc/html/_code_css.lua",
    ["ldoc.html._reset_css"] = "ldoc/html/_reset_css.lua",
    ["ldoc.html.ldoc_one_css"] = "ldoc/html/ldoc_one_css.lua",
    ["ldoc.html.ldoc_pale_css"] = "ldoc/html/ldoc_pale_css.lua",
    ["ldoc.html.ldoc_new_css"] = "ldoc/html/ldoc_new_css.lua",
    ["ldoc.html.ldoc_fixed_css"] = "ldoc/html/ldoc_fixed_css.lua",
    ["ldoc.builtin.globals"] = "ldoc/builtin/globals.lua",
    ["ldoc.builtin.coroutine"] = "ldoc/builtin/coroutine.lua",
    ["ldoc.builtin.global"] = "ldoc/builtin/global.lua",
    ["ldoc.builtin.debug"] = "ldoc/builtin/debug.lua",
    ["ldoc.builtin.io"] = "ldoc/builtin/io.lua",
    ["ldoc.builtin.lfs"] = "ldoc/builtin/lfs.lua",
    ["ldoc.builtin.lpeg"] = "ldoc/builtin/lpeg.lua",
    ["ldoc.builtin.math"] = "ldoc/builtin/math.lua",
    ["ldoc.builtin.os"] = "ldoc/builtin/os.lua",
    ["ldoc.builtin.package"] = "ldoc/builtin/package.lua",
    ["ldoc.builtin.string"] = "ldoc/builtin/string.lua",
    ["ldoc.builtin.table"] = "ldoc/builtin/table.lua",
  },
  copy_directories = {'tests'},
  install = {
    bin = {
      ldoc = "ldoc.lua"
    }
  }
}

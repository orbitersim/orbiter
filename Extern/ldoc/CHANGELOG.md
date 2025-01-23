## Version 1.5.0

### Features
  * Migrated to lunarmodules org, lowercased repository name, started using 'v' prefix for tags
  * Add Dockerfile and publish build to ghcr.io
  * Configure repository for use *as* a GitHub Action
  * Support CommonMark via cmark-lua processor
  * Add `--version` and improve CLI `--help` output
  * Add 'new' theme
  * Add filetype for xLua's .cs files
  * Add options for project icons & favicons
  * Distinguish between static/class members in moonscript
  * Add support for Lua 5.4
  * Add feature to skip adding timestamps to output
  * Prettify Lua function names
  * Make `@param` tag multiline
  * Allow using `@module`, `@script`, `@file`, etc. multiple times in a single source file
  * Expand builtin tparam aliases to full type names
  * Add `--unsafe_no_sandbox` argument and allow importing Lua modules from config
  * Support LLS-style tags for `@param` and `@return`

### Fixes
  * Correct documentation typos and errors in example code
  * Honor TMPDIR environment variable
  * Look for absolute references for `@see` references using module names first
  * Use default MD template if none specified
  * Recognize backslash separators in moonscript
  * Do not remove `@see` references from internal data structure
  * Fix Lua 5.1 support
  * Cleanup CLI input to remove trailing slashes

## Version 1.4.6

https://github.com/lunarmodules/ldoc/compare/1.4.5...1.4.6

## Version 1.4.5

https://github.com/lunarmodules/ldoc/compare/1.4.4...1.4.5

## Version 1.4.4

### Features
  * better Lua 5.3 support
  * handles tables with integer_keys as well as string keys
  * `--testing` - version and date does not change
  * `--no_args_infer` to completely switch off argument inference and parsing
  * `custom_csv` option to specify extra CSS file
  * explicit warnings if we cannot deduce item from following code
  * modules may return a single _function_ (see tests/funmod.lua)
  * honours the `SOURCE_DATE_EPOCH` environment variable
  * Moonscript fat arrow and auto-assign ctor support
  * Better Discount support. All varieties including Debian lua-discount package (binding to libmarkdown2)
  * prettier output for `ldoc -m` on Unix
  * updated global builtin documentation


### Fixes
  * consistently using Unix line-endings
  * `--fatalwarnings` now also works with parser errors (#255)
  * user-specific temporary directory for expanding templates
  * blank line after comment of code block suppressed (#240)
  * comments at end of code blocks were not highlighting
  * strip left spaces from usage code (#191)
  * don't parse `module` if it's a field name (e.g `foo.module()`)
  * temporary files on Windows fix

## Version 1.4.3

### Features

  * @include tag for including Markdown documentation file directly into module docstring
  * `prettify_files` makes per-item links to prettified source.
  * link targets rendered in bright yellow to make referenced functions more obvious
  * add update time to footer of page
  * better C support: `global_lookup=true` - invoked when `parse_extra={C=true}`
  * `kind_names` can override names used in sidebar

### Fixes

  * `all=true` in `config.ld` did not work.
  * `dont_escape_underscore` logic fixed: do not use in prettified code blocks
  * check that `ldoc` config exists before checking field values
  * annotation rendering fixed
  * summary not dropped when using `type` sections
  * directory as argument case was broken
  * parameter names which were List methods causing mayhem
  * files are processed in fixed order across platforms

## Version 1.4.2

### Features

  * Can define fields/properties of objects; `readonly` modifier supported (#93)
  * Can switch off auto-linking to Lua manual with `no_lua_ref`
  * Module sorting is off by default, use `sort_modules=true`
  * References to 'classes' now work properly
  * Option to use first Markdown title instead of file names with `use_markdown_titles`
  * Automatic `Metamethods` and `Methods` sections generated for `classmod` classes
  * `unqualified=true` to strip package names on sidebar (#110)
  * Custom tags (which may be hidden)
  * Custom Display Name handlers

### Fixes

  * stricter about doc comments, now excludes common '----- XXXXX ----' pattern
  * no longer expects space after `##` in Markdown (#96)
  * Section lookup was broken
  * With `export` tag, decide whether method is static or not
  * `classmod` classes now respect custom sections (#113)
  * Minor issues with prettification
  * Command-line flags set explicitly take precedence over configuration file values.
  * Boilerplate Lua block comment ignored properly (#137)
  * Inline links with underscores sorted (#22)
  * Info section ordering is now consistent (#150)

## Version 1.4.0

### Features

  * `sort=true` to sort items within sections alphabetically
  * `@set` tag in module comments; e.g, can say `@set sort=true`
  * `@classmod` tag for defining modules that export one class
  * can generate Markdown output
  * Can prettify C as well as Lua code with built-in prettifier
  * lfs and lpeg references understood
  * 'pale' template available
  * multiple return groups
  * experimental `@error` tag
  * Moonscript and plain C support


### Fixes

  * works with non-compatibily Lua 5.2, including `markdown.lua`
  * module names can not be types
  * all `builtin` Lua files are requirable without `module`
  * backticks expand in copyright and other 'info' tabs
  * `-m` tries harder to resolve methods
  * auto-scroll in navigation area to avoid breaking identifiers
  * better error message for non-luadoc-compatible behaviour
  * custom see references fixed




# LDoc - A Lua Documentation Tool

[![Luacheck](https://github.com/lunarmodules/ldoc/workflows/Luacheck/badge.svg)](https://github.com/lunarmodules/ldoc/actions)

Copyright (C) 2011-2012 Steve Donovan.

## Rationale

This project grew out of the documentation needs of
[Penlight](https://github.com/lunarmodules/Penlight) (and not always getting satisfaction
with LuaDoc) and depends on Penlight itself. (This allowed me to _not_ write a lot of code.)

The [API documentation](https://lunarmodules.github.io/Penlight/) of Penlight
is an example of a project using plain LuaDoc markup processed using LDoc.

LDoc is intended to be compatible with [LuaDoc](https://keplerproject.github.io/luadoc/) and
thus follows the pattern set by the various \*Doc tools:

    --- Summary ends with a period.
    -- Some description, can be over several lines.
    -- @param p1 first parameter
    -- @param p2 second parameter
    -- @return a string value
    -- @see second_fun
    function mod1.first_fun(p1,p2)
    end

Tags such as `see` and `usage` are supported, and generally the names of functions and
modules can be inferred from the code.

LDoc is designed to give better diagnostics: if a `@see` reference cannot be found, then the
line number of the reference is given.  LDoc knows about modules which do not use `module()`
- this is important since this function has become deprecated in Lua 5.2. And you can avoid
having to embed HTML in comments by using Markdown.

LDoc will also work with Lua C extension code, and provides some convenient shortcuts.

An example showing the support for named sections and 'classes' is the [Winapi
documentation](https://stevedonovan.github.io/winapi/api.html); this is generated from
[winapi.l.c](https://github.com/stevedonovan/winapi/blob/master/winapi.l.c).

## Installation

This is straightforward; the only external dependency is
[Penlight](https://github.com/lunarmodules/Penlight), which in turn needs
[LuaFileSystem](https://lunarmodules.github.io/luafilesystem/). These are already present
in [Lua for Windows](https://github.com/rjpcomputing/luaforwindows), and Penlight is also available through [LuaRocks](https://luarocks.org/) as `luarocks install
penlight`.

Unpack the sources somewhere and make an alias to `ldoc.lua` on your path. That is, either
an executable script called 'ldoc' like so:

    lua /path/to/ldoc/ldoc.lua $*

Or a batch file called 'ldoc.bat':

    @echo off
    lua \path\to\ldoc\ldoc.lua %*


## Generating LDoc on github

To generate docs for your own lua projects see [doc.yml](.github/workflows/doc.yml).

Instead of `luarocks install --only-deps ...`, use `luarocks install ldoc`
and create your own `doc-site` makefile target that runs `ldoc .` in the
directory containing your `config.ld`.

Ensure `publish_dir` in your doc.yml is set to the same location as your
`config.ld`'s `dir` parameter.

After you've pushed that change to master, you'll see the build cycle on your
commit (an orange dot or green checkmark). When that completes, a repo owner
needs to enable gh-pages on the repository: Settings > Pages and set "Source" to
gh-pages and root.

## Docker

Alternatively LDoc can be run as a standalone docker container.
The usage of docker is fairly simple.
You can either build your own or download a prebuilt version.
To build your own, execute the following command from the source directory of this project:

```console
$ docker build -t ghcr.io/lunarmodules/ldoc:HEAD .
```

To use a prebuilt one, download it from the GitHub Container Registry.
Here we use the one tagged *latest*, but you can substitute *latest* for any tagged release.

```console
$ docker pull ghcr.io/lunarmodules/ldoc:latest
```

Once you have a container you can run it on one file or a source tree (substitute *latest* with *HEAD* if you built your own or with the tagged version you want if applicable):

```console
# Run in the current directory
$ docker run -v "$(pwd):/data" ghcr.io/lunarmodules/ldoc:latest .
```

A less verbose way to run it in most shells is with at alias:

```console
# In a shell or in your shell's RC file:
$ alias ldoc='docker run -v "$(pwd):/data" ghcr.io/lunarmodules/ldoc:latest'

# Thereafter just run:
$ ldoc .
```
### Use as a CI job

There are actually many ways to run LDoc remotely as part of a CI work flow.
Because packages are available for many platforms, one way would be to just use your platforms native package installation system to pull them into whatever CI runner environment you already use.
Another way is to pull in the prebuilt Docker container and run that.

As a case study, here is how a workflow could be setup in GitHub Actions:

```yaml
name: LDoc
on: [ push, pull_request ]
jobs:
  sile:
    runs-on: ubuntu-latest
    steps:
      - name: Checkout
        uses: actions/checkout@v3
      - name: Generate docs with LDoc
        uses: lunarmodules/ldoc@v0
```

By default the GH Action is configured to run `ldoc .`, but you can also pass it your own `args` to replace the default input of `.`.

```yaml
      - name: Generate docs with LDoc
        uses: lunarmodules/ldoc@v0
        with:
            args: myfile.lua
```

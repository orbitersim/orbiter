# How to add new dependency

If you want to add new dependency, the way to add it will depend on the type

## Lua module

Lua modules are vendored as source directories in `Extern/` (`ldoc`, `Penlight`, `luafilesystem`):

1. Copy the module's sources into a new directory under `Extern/`
1. Pure-Lua modules: add a `copy_directory` command to the `CopyLDoc` target in Extern/CMakeLists.txt
1. Modules with C sources: build them as a library in Extern/CMakeLists.txt, as `lfs` is for luafilesystem

## C++ dependency

1. Create a new directory (e.g. `mylib`)
1. Add a line to Extern/CMakeLists.txt: `add_subdirectory(mylib)`
1. Copy `Extern/zlib/CMakeLists.txt` to new directory
1. Edit the file replacing `zlib` with `mylib` and adding new repository URL
1. Add other tweaks to `CMakeLists.txt` as necessary

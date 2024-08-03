# How to add new dependency

If you want to add new dependency, the way to add it will depend on the type

## lunarmodule dependency

Use `add_lunarmodule` function in Extern/CMakeLists.txt with appropriate tag:

```
add_lunarmodule(ldoc v1.5.0)
```

## C++ dependency

1. Create a new directory (e.g. `mylib`)
1. Add a line to Extern/CMakeLists.txt: `add_subdirectory(mylib)`
1. Copy `Extern/zlib/CMakeLists.txt` to new directory
1. Edit the file replacing `zlib` with `mylib` and adding new repository URL
1. Add other tweaks to `CMakeLists.txt` as necessary

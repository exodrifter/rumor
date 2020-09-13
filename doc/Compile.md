# Compiling Rumor

This document describes how to compile Rumor into a native shared library.

A shared library can be used by other programming languages other than Haskell.
A _native_ shared library can only be used on the target platform.

Right now, cross-compilation is not supported. This means if you would like to
make a shared library for Windows, you must run the compilation steps on
Windows.

* [Windows Instructions](#windows)

## Windows

These instructions assume a basic familiarity of PowerShell and are written
using Powershell.

First, navigate to the root of this project. Finally, build the project using
the following command:

```sh
stack build --ghc-options "-shared -fno-shared-implib -o Rumor.dll"
```
* `--ghc-options` tells stack to pass the following quoted arguments to GHC.
* `-shared` tells GHC to build a DLL instead of an executable.
* `-fno-shared-implib` tells GHC to not generate an import library for the DLL
  named `*.dll.a`. Generally, import libraries are only used to fix circular
  dependencies in builds so you probably won't need the import library. If you
  _do_ need the import library for some reason, then omit this flag.
* `-o Rumor.dll` tells GHC to name the DLL `Rumor.dll`. You can change this to
  any name you want, or leave it out to get the default name `HsDll.dll`.

A native `*.dll` file will be created which can be used in other languages that
support the [`__cdecl`][] calling convention such as C#.

You can use a program such as [dependency walker][] to confirm that the exported
Rumor functions from [`src/Rumor/FFI.hs`][] have been exported properly.

[`__cdecl`]: https://docs.microsoft.com/en-us/cpp/cpp/cdecl?view=vs-2019
[`src/Rumor/FFI.hs`]: /src/Rumor/FFI.hs
[dependency walker]: https://dependencywalker.com/

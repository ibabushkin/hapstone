# Hapstone - Haskell bindings for Capstone
This repository contains bindings to the Capstone disassembly framework
([link](https://github.com/aquynh/capstone)). At the time of writing,
both low-level and idiomatic bindings are present.

The documentation is probably pretty complete, but might need
clarification. In case you find bugs, want to contribute, especially
enhance the docs, please do this over issues/pull requests on GitHub,
contribution is always welcome.

[Link to Hackage](https://hackage.haskell.org/package/hapstone).

## Installation
To install the bindings, make sure `c2hs` is present on your system, as it
is used to preprocess the bindings to Capstone. Obviously, Capstone needs
to be installed as well. Then, you can simply pull the package from Hackage
via `cabal` or manually.

## Examples
Please see the examples directory for more information on how to use Hapstone.

In short, you must import `Hapstone.Capstone` and then call either `disasmIO` or `disasmSimpleIO` if you plan on using the `defaultAction` rather than building your own. You define a `Disassembler` with the correct parameters that define the architecture and mode to use, plus the actual buffer of data that should be disassembled.

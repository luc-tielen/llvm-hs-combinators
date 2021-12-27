# llvm-hs-combinators

A Haskell package containing combinators that are useful when generating LLVM
code using the [llvm-hs-pure](https://github.com/llvm-hs/llvm-hs.git) library.

## Getting started

1. Add `llvm-hs-combinators` to your package.yaml dependencies,
2. Point llvm-hs-pure to a recent commit (e.g. the llvm-9 branch or newer),
3. Import `LLVM.IRBuilder.Combinators` in your Haskell code!

For an example of how to do this, see
[here](https://github.com/luc-tielen/eclair-lang/blob/ef514ddbe4cacea91b040b0f2746420e6f1f8dcd/package.yaml#L34)
and [here](https://github.com/luc-tielen/eclair-lang/blob/ef514ddbe4cacea91b040b0f2746420e6f1f8dcd/stack.yaml#L53-L54).

## Versioning

Unlike llvm-hs / llvm-hs-pure, this library does not take the LLVM version into
account, since it only makes use of the Haskell functions exposed by the
libraries (that are unlikely to change).

Right now, this library is not on Hackage due to llvm-hs-pure not having a release
that contains the improved handling of terminators in the IRBuilder. As soon as
this is done, this library can be released to Hackage as well. You can however
still use this library if you pull it directly from Github!

This library is a pure-Haskell implementation of the [slaw-v2](https://purl.org/funwithsoftware/slaw-v2) data serialization format, whose canonical implementation is in the C library [libPlasma](https://purl.org/funwithsoftware/libPlasma).

For "pool" functionality, see the [hs-plasma](https://github.com/mignon-p/hs-plasma) package, which wraps the libPlasma C library.

Some differences from the C implementation of libPlasma:

* Although multivectors are minimally supported, there is not a comprehensive set of types for them.
* The Haskell implementation supports 16-bit floating point, which is not currently supported by the C implementation.
* The Haskell implementation supports a "symbol" type, which is basically a generalization of the existing "true", "false", and "nil".

## Building

To build both the hs-slaw and [hs-plasma](https://github.com/mignon-p/hs-plasma) libraries, see the [hs-plasma](https://github.com/mignon-p/hs-plasma) repo.

To build the hs-slaw library alone, just build as normal with either Cabal or Stack.

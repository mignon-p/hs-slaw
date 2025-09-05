This library is a pure-Haskell implementation of the
[slaw-v2](https://purl.org/funwithsoftware/slaw-v2) data serialization
format, whose canonical implementation is in the C library
[libPlasma](https://purl.org/funwithsoftware/libPlasma).

For "pool" functionality, see the
[hs-plasma](https://github.com/mignon-p/hs-plasma) package, which
wraps the libPlasma C library.

Pre-built Haddock documentation for `hs-slaw` and `hs-plasma` is
available [here](https://funwithsoftware.org/hs-plasma/).

Some differences from the C implementation of libPlasma:

* Although multivectors are minimally supported, there is not a
  comprehensive set of types for them.
* The Haskell implementation supports 16-bit floating point, which is
  not currently supported by the C implementation.
* The Haskell implementation supports a "symbol" type, which is
  basically a generalization of the existing "true", "false", and
  "nil".

## Serializing data to and from slawx

Slawx are represented with the `Slaw` datatype.  The `ToSlaw` and
`FromSlaw` typeclasses make it easy to convert between `Slaw` and
other types.

For brevity, `š` is provided as a synonym for `toSlaw`.  As a
mnemonic, the arrow above the `s` points *towards* the `s`.  `š`
always succeeds, so there is no need for error handling.

For converting from a `Slaw` to another type, the `ŝ` function is
provided.  As a mnemonic, the arrow above the `s` points *away from*
the `s`.  If the `Slaw` cannot be converted to the desired type, a
`PlasmaException` is thrown.

Several variants of `ŝ` are provided with different error handling
behavior.  For example, `ŝm` returns a `Maybe`, while `ŝes` and `ŝee`
return an `Either`.

To make it easier to type `š` and `ŝ`, you can add the following code
to your `.emacs` file:

```elisp
(defun fws-insert-from-slaw ()
  (interactive)
  (insert "ŝ"))

(defun fws-insert-to-slaw ()
  (interactive)
  (insert "š"))

(global-set-key (kbd "C-x <up>")   'fws-insert-from-slaw)
(global-set-key (kbd "C-x <down>") 'fws-insert-to-slaw)
```

Then you can use `C-x <up>` to insert `ŝ`, or `C-x <down>` to insert
`š`.

## Building

To build both the hs-slaw and
[hs-plasma](https://github.com/mignon-p/hs-plasma) libraries, see the
[hs-plasma](https://github.com/mignon-p/hs-plasma) repo.

To build the hs-slaw library alone, just build as normal with either
Cabal or Stack.

You need to have `perl` on your `PATH`, because Perl is used to
preprocess some source files.  (Files with the extension
[.hs-template](doc/template.txt))

## License

`hs-slaw` is licensed under the [MIT License](LICENSE).
© Mignon Pelletier, 2024-2025.

Some documentation text has been taken from
[libPlasma](https://purl.org/funwithsoftware/libPlasma),
© oblong industries.

# About

`smtpp` is a preprocessor for SMT-LIB 2.0 scripts with extensions for
lambda-terms and polymorphic types in scripts.

# Dependencies

- OCaml
- [menhir](http://gallium.inria.fr/~fpottier/menhir/)

For developers only:
- headache for license header generation


# Compilation

The usual incantation

```{.bash}
% ./configure
% make depend && make
```

should produce two binaries in `src`
- `smtpp.byt` : a bytecode executable software
- `smtpp.opt` : a native code executable


# Documentation

Options are detailed on stdout by executing `smtpp.opt -help` or
`smtpp.byt -help`.


<!--  LocalWords:  smtpp
 -->

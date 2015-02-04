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
% make
```

should produce two binaries in `src`
- `smtpp.byt` : a bytecode executable software
- `smtpp.opt` : a native code executable


## Developers

If you are working in the `src` directory, you might not want to recompile
everything for every change.

If there is no `.depend` file, do a `touch .depend` first.
Otherwise:

- If you have just changed a file and not added any, a simple `make` should
suffice
- If you have added a file, add it to the Makefile in the proper file list, and
  do a `make depend && make`.

# Documentation

Options are detailed on stdout by executing `smtpp.opt -help` or
`smtpp.byt -help`.


<!--  LocalWords:  smtpp
 -->

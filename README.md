# Euclidea
A demo using OpenGL to re-create graphical effects from noneuclidean games, such as Antichamber and Manifold Garden.

## Running
The program can be run from the root directory if you have a common lisp installation with quicklisp, e.g. `sbcl` with 
```sh
sbcl --load "main.lisp"
* (main)
```
Alternatively, the `build` directory contains windows and linux binaries. These *must* be run from the root directory (=euclidea=), because they look for the =resouces= folder via a relative path.

## Documentation
Most documentation is in the form of docstrings/comments in the source code, with the 'important' code being in `custom/portal.lisp`. In addition, there is a document `Portal-Math.pdf`, which contains a basic explanation of the math. In general, I expect the reader to be familiar with elements from linear algebra, such as projection/view matrices, basic transformation matrices, cross-products, etc.

## Non-Features
This is not intended to be a 'useful' game/program, merely an experiment/demo demonstrating what's possible & how to do it. Feel free to build on the program/take inspiration from what I've done here. Happy coding :)

# Euclidea
An attempt to use OpenGL to re-create graphical effects from noneuclidean games, e.g. Antichamber and Manifold Garden. This effect is primarily achieved through seamless 'portals'.

## Running
The program does not currently use asdf (sorry), but, you should be able to load the main file (e.g. `sbcl --load "main.lisp"`) from the root directory, and everything will behave nicely. Currently, it is only tested on SBCL. For those that do not have a lisp installed, there are binaries available in the `build` directory. If you run these, you MUST ensure that the working directory of the program is the root directory, otherwise they won't be able to find the needed
resource files.

## Documentation
Most documentation is in the form of docstrings/comments in the source code, with the 'important' code being in `custom/portal.lisp`. In addition, there is a document `Portal-Math.pdf`, which contains a basic explanation of the math. In general, I expect the reader to be familiar with the basic mathematics behind rasertization, i.e. linear algebra, e.g. projection/view matrices, transformation matrices, cross-product, etc.

# Features/TODO
## Currently Unimplemented
- [ ] Pretty Shaders
- [ ] Teleportation through portals
- [ ] Lighting through portals
- [ ] Informative documentation, particularly on portal mathematics (WIP)

## Features/Completed
- [x] Rendering to Framebuffer
- [x] Framebuffer as Texture
- [x] Camera changes in portal-plane
- [X] Arbitrary portal direction & direction fixes 

## Non-Features
This is not intended to be a 'useful' game/program, merely an experiment/demo demonstrating what's possible & how to do it. Feel free to build on the program/take inspiration from what I've done here. Happy coding :)

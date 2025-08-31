# PrEd

PrEd is a Proof Editor, an IDE
specifically tailored for interactive proof assistants.

## How to build

`sdl2`, `SDL2_ttf` and `fontconfig` should be discoverable by `pkgconfig`.
If they are, run
```bash
$ cabal build
```
and you're good to go.

## How to run

For now, we use Fira Code as a main font because I like it.
So it should be discoverable by `fontconfig`; if it is, run
```bash
$ cabal run pred -- FILE
```
and you're good to go.

## TODO list

- [x] modes
- [x] cursor
- [ ] basic editing
- [x] lazy rendering
- [ ] file watching with `fsnotify`
- [ ] event multiplexing with channels
- [ ] font picker

# Elmentory

All of the Elm code lives in `src/Main.elm` and relies on the [elm/html][html] library.

[html]: https://package.elm-lang.org/packages/elm/html/latest

There also is a port handler set up in `index.html` to store the Elm application's state in `localStorage` on every update.


## Build Instructions

Run the following command from the root of this project:

```bash
elm make src/Main.elm --output=elm.js
```

Then open `index.html` in your browser!

## Electron

Run as an application:

```bash
electron index.html
```

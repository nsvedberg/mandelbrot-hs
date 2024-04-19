# mandelbrot-hs

ITCS 4102 Group 2: *The Strongly Typed*

Generate an image of the famous Mandelbrot set fractal using SDL2.

![Example screenshot](/docs/Screenshot.png)

## Requirements

You will need Haskell's `cabal` build system. The easiest way to set up Haskell is with [GHCup](https://www.haskell.org/ghcup/).

You will also need SDL2. For Arch:

```bash
sudo pacman -S sdl2
```

or for Debian-based:

```bash
sudo apt install libsdl2-dev
```

## Usage

To build:

```bash
cabal build
```

To run:

```bash
cabal run
```

## Controls

| Key | Description |
| :-: | :- |
| Q | Quits the program |
| S | Saves a screenshot of the current render to the `screenshots/` folder |
| [NUM] | Changes fractal mode. `1`: Mandelbrot, `2`: Julia Set |
| ↑ | Zoom in |
| ↓ | Zoom out |

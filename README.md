# mandelbrot-hs

ITCS 4102 Group 2: *The Strongly Typed*

Generate an image of the famous Mandelbrot set fractal using SDL2.

![Example screenshot](/docs/Screenshot.png)

## Requirements

You will need Haskell's `cabal` build system. The easiest way to set up Haskell is with [GHCup](https://www.haskell.org/ghcup/).

After you have GHCup installed, you will need to run `ghcup tui` and make sure you have `GHC 9.2.8 base-4.16.4.0` installed and selected for use.

You will also need SDL2 from your system's package manager. For Arch:

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
| [CLICK] | Clicking a point on the screen moves the viewport to focus on that point |
| + | Zoom in |
| - | Zoom out |
| ? | Show a help screen |

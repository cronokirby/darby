# darby

[![Hackage](https://img.shields.io/hackage/v/darby.svg)](https://hackage.haskell.org/package/darby)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/cronokirby/darby/blob/master/LICENSE)



Simple music shuffler and player. `darby` simply
takes a directory, pulls out all the `mp3` files in it,
shuffles them, and then plays them one by one.

This is a tool I made because it sits well with my workflow
of just setting up a bunch of songs to play
in the background while working on something else.

`darby` plays well with 
[populate](https://github.com/cronokirby/populate),
another tool I made.


## Changelog
[List of changes](CHANGELOG.md)


## Building

Darby relies on `sdl2` and `sdl2-mixer` for playing audio,
so the first step is to install those dependencies.

### Ubuntu
```
sudo apt install libsdl2-dev libsdl2-mixer-dev
```

This should be similar on other platforms.
Feel free to contribute with build instructions for your
platform :)


After that, stack can be used to build and install the
program:
```
stack install
```

## Usage
```
Usage: darby DIRECTORY [--noshuffle] [--display]
  Shuffle and play the songs in FILE

Available options:
  --noshuffle              Set this to not shuffle the songs
  --display                Print out the list without playing it
  -h,--help                Show this help text
```

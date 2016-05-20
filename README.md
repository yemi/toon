# Toon

The SoundCloud based interactive radio station

## Installation

Toon uses `mpd` (music player daemon) to stream audio in a headless manner.

```shell
$ brew tap yemi/toon
$ brew install mpd toon
```

## Usage

```shell
$ mpd --no-config # runs the mpd server that toon interacts with

$ toon
```

Controls (Vim style):

`h` - previous

`j` - pause

`k` - play

`l` - next

`r` - enter track URL

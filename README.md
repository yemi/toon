# Toon

The SoundCloud based interactive radio station

## Installation

Toon uses `mpd` (music player daemon) to stream audio in a headless manner.

```
$ brew tap yemi/toon
$ brew install mpd toon
```

## Usage

```
$ mpd --no-config # runs the mpd server

$ toon
```

Controls (Vim style):

`h` - previous

`j` - pause

`k` - play

`l` - next

`r` - enter track URL

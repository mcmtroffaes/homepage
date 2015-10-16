---
title: Converting and Merging Videos Using ffmpeg
author: Matthias C. M. Troffaes
tags: ffmpeg, nix, youtube
---

The Problem
-----------

Recently, I found myself creating a screencast on a mathematical
subject. This was great fun, and I would recommend it to anyone
teaching maths.

A practical problem you will run into is that, at some point, you find
yourself having a number of video files in different formats, with
different encodings, different bit rates, even different resolutions,
which you want to merge together into a single file for upload to
youtube. It took quite a bit of research to do this effectively, so
here I share you my method, with some explanation of all the flags
that are used in the encoding process, to get a file that fits
YouTube's [recommended upload encoding
settings](https://support.google.com/youtube/answer/1722171?hl=en-GB).

Encoding With Youtube's Recommended Settings
--------------------------------------------

First, we encode each input video file into the recommended format.
Here is a shell function that one can use for this purpose:

``` {.sourceCode .bash}
convert() {
    echo "converting $1 into $2..."
    ffmpeg -v warning -i "$1" \
    -r 60 -s 1920x1080 \
    -vf "scale=iw*min(1920/iw\,1080/ih):ih*min(1920/iw\,1080/ih),pad=1920:1080:(1920-iw)/2:(1080-ih)/2" \
    -c:a libfdk_aac -ac 2 -ar 48000 -b:a 384k \
    -movflags +faststart \
    -c:v libx264 -preset veryslow -crf 18 \
    "$2"
    echo "checking bitrate of $2 (if > 12 Mb/s, re-encode with a higher -crf value)"
    ffmpeg -i "$2" 2>&1 | grep bitrate
}
```

Let us discuss the arguments to ffmpeg.

* ``-r 60 -s 1920x1080``: 60 frames per second, 1920x1080 resolution
  (high definition). If all of your sources have a lower frame
  rate---30 is quite typical---then you can go for a lower frame rate.

* ``-vf "scale=iw*min(1920/iw\,1080/ih):ih*min(1920/iw\,1080/ih),pad=1920:1080:(1920-iw)/2:(1080-ih)/2"``:
  This pads your video with black bands if need be, to ensure a
  correct aspect ratio in the output file. See [this post](https://stackoverflow.com/questions/8133242/ffmpeg-resize-down-larger-video-to-fit-desired-size-and-add-padding)
  on stackoverflow.

* ``-c:a libfdk_aac``: Use the [Fraunhofer FDK AAC
  codec](https://trac.ffmpeg.org/wiki/Encode/AAC#fdk_aac). This codec
  is non-free, so we compile our own version of ffmpeg in order to use
  it. A nix expression to achieve this is at the end of this post.

* ``-ac 2 -ar 48000 -b:a 384k``: Stereo, 48kHz, 384k bitrate.

* ``-movflags +faststart``: Ensure the audio stream is [streaming
  friendly](https://trac.ffmpeg.org/wiki/Encode/AAC#fdk_stream).

* ``-c:v libx264 -preset veryslow -crf 18``: Use the H.264 codec, with
  the slowest preset (this gives the best quality), at a high quality
  rate factor of 18.

The command ``ffmpeg -i $2 2>&1 | grep bitrate`` prints the bit rate
after conversion. If you get values over 12 Mbps then you should
increase the rate factor for this specific video.

Merging
-------

First, create a file called ``files.txt`` which contains all output
files from the previous step; for example:

```
file 'video1.mp4'
file 'video2.mp4'
file 'video3.mp4'
file 'video4.mp4'
file 'video5.mp4'
```

Now call ffmpeg to concatenate these files into a single video:

``` {.sourceCode .bash}
ffmpeg -v warning -f concat -i files.txt -c copy -flags +global_header videofinal.mp4
```

The option ``-flags +global_header`` fixes a warning
with the current version of ffmpeg;
see [this bug report](https://trac.ffmpeg.org/ticket/4528).

Now you're done!

Compiling ffmpeg With libfdk_aac
--------------------------------

You could download the ffmpeg source code, install all dependencies,
and so on, but this takes a considerable amount of effort.
Instead of doing this manually, let's use [nix](https://nixos.org/nix/)!

Without further ado, here is the nix expression:

``` {.sourcecode .nix}
{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  myffmpeg = pkgs.ffmpeg-full.override {
      nonfreeLicensing = true;
      fdkaacExtlib = true;
      fdk_aac = pkgs.fdk_aac;
  };
in
pkgs.stdenv.mkDerivation {
  name = "my-ffmpeg-env";
  buildInputs = [ myffmpeg ];
}
```

Put this in a ``shell.nix`` file, and run in your terminal:

``` {.sourcecode .bash}
NIXPKGS_ALLOW_UNFREE=1 nix-shell
```

This will download all dependencies and compile your own version of
ffmpeg.

Have a look at the nix manual
for more information about
[how to override package arguments](https://nixos.org/nixpkgs/manual/#sec-pkg-override)
and
[how to install unfree licensed packages](https://nixos.org/nixpkgs/manual/#chap-packageconfig).
The full set of arguments for the ``ffmpeg-full`` nix package
can be found in the [sourcecode](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/libraries/ffmpeg-full/default.nix).

So easy... Nix is awesome!

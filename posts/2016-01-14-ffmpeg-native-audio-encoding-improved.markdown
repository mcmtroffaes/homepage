---
title: FFmpeg Native Audio Encoding Improved
author: Matthias C. M. Troffaes
tags: ffmpeg, nix
---

Native AAC Encoding
-------------------

A few months ago, we discussed how to compile our own version of
ffmpeg with nix to get high quality audio encoding with
libfdk_aac. Whilst nix makes this quite easy, since last December,
ffmpeg's native AAC encoder has improved sufficiently so it will no
longer be necessary to compile your own non-free version of
ffmpeg. From the
[ffmpeg website](https://www.ffmpeg.org/#aac_encoder_stable):

> After seven years the native FFmpeg AAC encoder has had its
> experimental flag removed and declared as ready for general use. The
> encoder is transparent at 128kbps for most samples tested with
> artifacts only appearing in extreme cases. Subjective quality tests
> put the encoder to be of equal or greater quality than most of the
> other encoders available to the public.
>
> Licensing has always been an issue with encoding AAC audio as most
> of the encoders have had a license making FFmpeg unredistributable
> if compiled with support for them. The fact that there now exists a
> fully open and truly free AAC encoder integrated directly within the
> project means a lot to those who wish to use accepted and widespread
> standards.
>
> The majority of the work done to bring the encoder up to quality
> was started during this year's GSoC by developer Claudio Freire and
> Rostislav Pehlivanov. Both continued to work on the encoder with the
> latter joining as a developer and mainainer, working on other parts
> of the project as well. Also, thanks to Kamedo2 who does comparisons
> and tests, the original authors and all past and current
> contributors to the encoder. Users are suggested and encouraged to
> use the encoder and provide feedback or breakage reports through our
> bug tracker.

Do You Have It?
---------------

Any release containing
[this patch](https://git.videolan.org/?p=ffmpeg.git;a=commitdiff;h=e34e3619a2b5b6fb4b4d9e68504b528c168da868),
which removes the experimental flag on the native AAC encoder,
will be good to go.

To figure out if it works for your version, simply run:

``` {.sourcecode .bash}
ffmpeg -i input.wav -c:a aac test.aac
```

where ``input.wav`` can be any audio file that you happen to have on
your system. If this works, then you have the patch. If this tells you

> The encoder 'aac' is experimental but experimental codecs are not
> enabled, add '-strict -2' if you want to use it.

then you are out of luck. FFmpeg 2.8.4 does not yet have the
patch, so for now you have to grab a git build
[for linux](http://johnvansickle.com/ffmpeg/)
or [for windows](http://ffmpeg.zeranoe.com/builds/).

Compiling FFmpeg From Git
-------------------------

Although obviously it is much easier to grab a pre-built git version,
as an exercise,
we can also use [nix](https://nixos.org/nix/) to try to build our own
version from git.
Here is the nix expression:

``` {.sourcecode .nix}
{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  myffmpeg = pkgs.ffmpeg-full.overrideDerivation (oldAttrs: rec {
    version = "62dfe1d4";  # git hash
    src = pkgs.fetchFromGitHub {
      owner = "FFmpeg";
      repo = "FFmpeg";
      rev = "${version}";
      sha256 = "1gnlny873r5qagkn5ybvbxv94yhm0dlwslhbm28af6if8j7pjcp7";
    };
  });
in
pkgs.stdenv.mkDerivation {
  name = "my-ffmpeg-env";
  buildInputs = [ myffmpeg ];
}
```

The above code is simply
[overriding](https://nixos.org/nixpkgs/manual/#sec-pkg-overrideDerivation)
the standard
[ffmpeg-full](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/libraries/ffmpeg-full/default.nix)
derivation to grab the source from github.

Put this in a ``shell.nix`` file, and run ``nix-shell`` in your terminal.
This will build ffmpeg at git hash
[62dfe1d4](https://git.videolan.org/?p=ffmpeg.git;a=shortlog;h=62dfe1d4).
You now have your own non-experimental AAC enabled ffmpeg!

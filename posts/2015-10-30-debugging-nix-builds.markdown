---
title: Debugging Nix Builds
author: Matthias C. M. Troffaes
tags: nix, mingw
---

Hello World
-----------

For fun, I tried out cross compiling with nix on my linux machine,
targetting windows. As a first attempt, we build GNU's hello world
application. Here's the ``shell.nix`` file:

``` {.sourceCode .nix}
with import <nixpkgs> {
  crossSystem = {
    config = "i686-w64-mingw32";
    arch = "i686";
    libc = "msvcrt";
    platform = { };
    openssl.system = "mingw";
  };
};
{
  my-env = stdenv.mkDerivation {
    name = "my-env";
    buildInputs = [ hello.crossDrv ];
  };
}
```

This results in a working executable:

```
$ wine /nix/store/k4zsl4x1xvb2d5pyf8f0nh7mhz33q5av-hello-2.10-i686-w64-mingw32/bin/hello.exe
Hello, world!
```

OpenSSL
-------

Let us do something a bit more challenging and
cross compile the OpenSSL library.
Here's a quick nix shell script
that should achieve exactly that:

``` {.sourceCode .nix}
with import <nixpkgs> {
  crossSystem = {
    config = "i686-w64-mingw32";
    arch = "i686";
    libc = "msvcrt";
    platform = { };
    openssl.system = "mingw";
  };
};
{
  my-env = stdenv.mkDerivation {
    name = "my-env";
    buildInputs = [ openssl.crossDrv ];
  };
}
```

Unfortunately, this fails with an error. The relevant part of the
error message is:

```
/bin/sh: line 17: windres: command not found

```

Debugging in a Local Nix Build Environment
------------------------------------------

We need to tell the openssl build where to find ``windres``.
Because we are cross compiling, ``windres`` is actually called
``i686-w64-mingw32-windres``. A bit of searching reveals a patch on the
bug tracker which allows the build to pass a custom windres binary.
We grab the [patch](https://github.com/niXman/mingw-builds/blob/master/patches/openssl/openssl-1.0.0d-windres.patch)
from the mingw-builds project, and now we would like to:

1. apply this patch to the openssl source code, and

2. add an environment variable so when nix configures openssl, the
   appropriate binary is found.

Because these changes are likely to be found useful by the upstream
nix project, we shall simply clone the nixpkgs repository and apply
our changes directly there. So, how do we go about this?

First, we start a simple nix shell for cross compiling, with a nix
stdenv set up for us for building openssl, so we do not need to repeat
the nix build process from scratch every time we make a change.
The nix shell script is very simple:

``` {.sourceCode .nix}
import <nixpkgs>
{
  crossSystem = {
    config = "i686-w64-mingw32";
    arch = "i686";
    libc = "msvcrt";
    platform = { };
    openssl.system = "mingw";
  };
}

```

Now the obscure command that sets up a shell with precisely the build
environment for openssl that nix would be using is as follows,
assuming your local nixpkgs git checkout resides in
``/home/user/nixpkgs/``:

``` {.sourceCode .bash}
NIX_PATH=nixpkgs=/home/user/nixpkgs/ nix-shell -A openssl.crossDrv
```

The environment variable ensures we are using our local copy of the
nixpkgs repository rather than the default channel.  In this
environment, we can reproduce the steps that nix is executing when
building openssl:

``` {.sourceCode .bash}
unpackPhase
cd openssl-1.0.1p
patchPhase
configurePhase
buildPhase
```

Obviously, this will result in the same error as before.  To fix
things, we go to the
``nixpkgs/pkgs/development/libraries/openssl``
folder, add our ``openssl-1.0.0d-windres.patch`` file,
and edit the ``default.nix`` script to apply the patch and to
[make the configure step use the correct windres executable by setting the ``WINDRES`` environment variable](https://github.com/mcmtroffaes/nixpkgs/commit/284366cc44fe2d47ed947d2b205b1984b2f3c0ad).

After applying this patch, we rerun our phases again, and we notice
another failure at the ``buildPhase``, while linking the test
executables:

```
../libssl.a: could not read symbols: Archive has no index; run ranlib to add one
collect2: error: ld returned 1 exit status
```

Apparently, the configure script sets ``RANLIB`` to ``true``, which
effectively does nothing with the file. If we [set the ``RANLIB``
environment variable for the build to find the correct ranlib
executable](https://github.com/mcmtroffaes/nixpkgs/commit/0b1464051cd7cda2a30b81a4d46252c0b167304a),
everything builds properly.

A [pull request with all patches is submitted upstream](https://github.com/NixOS/nixpkgs/pull/10790).

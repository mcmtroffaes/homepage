---
title: Running Hakyll With Nix
author: Matthias C. M. Troffaes
tags: hakyll, nix
---

The Problem
-----------

A classic annoying issue with Haskell is cabal's refusal to properly
resolve and install dependencies in sandboxes when you also have
system packages installed, especially when those system packages
satisfy some but not all of the dependencies in the chain.
In particular, cabal will sometimes compile
against multiple versions of the same package.
During the last few days, I repeatedly ran into the following error
when trying to install Hakyll 4.7.1.0:

``` {.sourceCode .bash}
cabal sandbox init
cabal install hakyll -j8
```

This eventually results into:

```
Configuring pandoc-1.14.1...
Failed to install pandoc-1.14.1
Build log ( /.../.cabal-sandbox/logs/pandoc-1.14.1.log ):
cabal: Error: some packages failed to install:
hakyll-4.7.1.0 depends on pandoc-1.14.1 which failed to install.
pandoc-1.14.1 failed during the configure step. The exception was:
user error ('/usr/bin/ghc' exited with an error:
/usr/lib/ghc-7.6.3/unix-2.6.0.1/libHSunix-2.6.0.1.a(execvpe.o):
In function `pPrPr_disableITimers':
execvpe.c:(.text+0x300): multiple definition of `pPrPr_disableITimers'
/.../.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/unix-2.7.1.0/libHSunix-2.7.1.0.a(ghcrts.o):(.text+0x0):
first defined here
collect2: error: ld returned 1 exit status
)
pandoc-citeproc-0.7.2 depends on pandoc-1.14.1 which failed to install.
```

So we see that pandoc somehow gets linked against both `unix-2.6.0.1`
(from the system installation) as well as against `unix-2.7.1.0`
(from the sandbox, most likely to satisfy another dependency of hakyll).

The solution is simple: isolate the sandbox from the system.
How can we achieve this? There's actually a programming language
and package manager written specifically to solve this problem.
It is called [Nix](https://nixos.org/nix/). In fact,
Nix can be used to build an entire operating system. The beauty of Nix is that
it can also run code locally in a way that is for all practical purposes
completely isolated from the rest of the system.

The information for using Haskell with Nix is vast;
good starting points are
[the nixos wiki](https://nixos.org/wiki/How_to_install_nix_in_home_%28on_another_distribution%29)
and
[the nixpkgs manual](http://hydra.nixos.org/job/nixpkgs/trunk/manual/latest/download-by-type/doc/manual#users-guide-to-the-haskell-infrastructure).
It is a bit hard to see the trees in the forest, so
this post will summarize the main points and walk through
the installation and basic use.

Setting Up Nix
--------------

Nix will store everything in the `/nix` folder at the root of your
file system.  If you have administrator access on your machine, then
you can skip the following step, but if like myself you do not have
root access, you can use the [proot](http://proot.me/) utility.
It is assumed that `~/.local/bin/` is in your `PATH`.

``` {.sourceCode .bash}
cd ~/.local/bin
wget http://portable.proot.me/proot-x86_64
chmod +x proot-x86_64 
```

Now we create a local folder for Nix, and bind it to the `/nix` folder:

``` {.sourceCode .bash}
mkdir ~/nix
proot-x86_64 -b ~/nix:/nix bash
```

Now we install Nix:

``` {.sourceCode .bash}
curl https://nixos.org/nix/install | sh
```

This will set up the following for you:

* `/nix` will contain all built packages

* `~/.nix-channels` tells Nix where to get its package descriptions from

* `~/.nix-defexpr/` contains all build and install instructions of every
  package, written in the nix language.

* `~/.nix-profile/` contains the local configuration, and in essence
  comes down to the nix filesystem root, containing mostly symbolic
  links to `/nix` where all the built files reside.

It will also add the following line to your `.bash_profile` file:

``` {.sourceCode .bash}
if [ -e ~/.nix-profile/etc/profile.d/nix.sh ]; then
    . ~/.nix-profile/etc/profile.d/nix.sh;
fi
```

To set up the environment every time that you need Nix,
you can use the following command:

``` {.sourceCode .bash}
proot-x86_64 -b ~/nix:/nix bash --login
```

The `--login` option ensures that your `.bash_profile` is read
when `bash` is started, which will in turn ensure that
the commands from `~/.nix-profile/etc/profile.d/nix.sh` are executed,
which in turn ensures that bash is set up properly for Nix.

We can save ourselves some typing by adding the following line to your
`.bashrc` file:

``` {.sourceCode .bash}
alias nixbash="proot-x86_64 -b ~/nix:/nix bash --login
```

Now you can simply type `nixbash` in bash to start a Nix-enabled bash.

To update your Nix environment to use the latest packages for everything,
run:

``` {.sourceCode .bash}
nix-channel --update
```

If this breaks something and you want to roll back to your previous
configuration, just run:

``` {.sourceCode .bash}
nix-channel --rollback
```

Using Nix's Hakyll
------------------

We write a Nix script to set up the right environment for us.
Save this as `shell.nix` in the root folder of your favourite Haskell project:

``` {.sourceCode .nix}
{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          hakyll  # list of Haskell packages here!
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "my-hakyll-env";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
```

Now, run `nix-shell` from the same folder (this will take a long time
on first invokation as it will need to download or build all dependencies),
and you have a fully isolated Hakyll library at
your fingertips!

Unicode Issues
--------------

The only problem I ran into is that Nix compiles highlighting-kate
with pcre-light. Now, highlighting-kate does not properly support
unicode when compiled that way. I [wrote and pushed a fix upstream](https://github.com/jgm/highlighting-kate/pull/74) and in fact it is
already merged.  As soon as Nix picks up the patch, which should be soon
after the next release of highlighting-kate, we will have a fully
functional Hakyll in Nix with proper unicode support.

In the mean time, you can install cabal-install with Nix (just replace
"hakyll" with "cabal-install" in the Nix shell script;
you will also need to specify "JuicyPixels"
under your Haskell package list and "pkgs.zlib" under `buildInputs`
to help cabal building Hakyll), and then use a
cabal sandbox from within Nix to build highlighting-kate locally along
with Hakyll:

``` {.sourceCode .bash}
cabal sandbox init
cabal install ~/src/highlighting-kate/ hakyll -j8
cabal exec ghc -- --make site.hs
```

Further Reading
---------------

I highly recommend reading [Luca Bruno's Nix pills](http://lethalman.blogspot.co.uk/2014/07/nix-pill-1-why-you-should-give-it-try.html),
which provide a thorough introduction to Nix from first principles.

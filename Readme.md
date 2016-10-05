Generator of URLs for downloading Debian repository indexes 
======
This program is the scratch to an itch. The itch is the lack
of network connection in my home computer, which makes harder
installing Debian packages. The source is clear enough (for my
standards), so you can read it with not much effort (it is a
literate program). Actually is meant to be read.

Building and installing
----
All you need is the Haskell Platform, it was developed with
GHC 8.0.1. Run cabal configure && cabal build && cabal install.

Usage
----
Typing debindex at the shell will show the help. An example
of invocation is 'debindex "http://debian.org" debian jessie\
main,contrib,non-free i386,amd64'.
This is a Haskell module for GLFW OpenGL framework
(http://glfw.sourceforge.net). It provides an alternative
to GLUT for OpenGL based Haskell programs.

SOE (http://www.haskell.org/soe) now depends on this package.

The website for this Haskell module is at Haskell Wiki site:
http://haskell.org/haskellwiki/GLFW

=======
Changes
=======

See separate file "Changlog.txt".

============
Installation
============

The package comes together with a (partial) source distribution 
of GLFW v2.6, which is compiled and installed together with
the Haskell package.

If you already have the Haskell package cabal-install, you can
simply do "cabal install GLFW", and it will download the latest
source from HackageDB, configure, compile, and install it 
automatically.

Otherwise, you may follow the standard Cabal package installation 
steps:

1. To configure the module, type

       runhaskell Setup.hs configure
   or

       runhaskell Setup.hs configure --user --prefix=DIR

   if you want to install the package to your user's directory
   instead of the system one (replace DIR with your own directory
   choice).

2. To build the module, type 

       runhaskell Setup.hs build

3. To install, type 

       runhaskell Setup.hs install   

In the process it builds all GLFW C library source code. You may
use "runhaskell Setup.hs build --verbose" to see the actual 
compilation steps.

4. Optionally to build its Haddock documentation, type

       runhaskell Setup.hs haddock

====
NOTE
====

For Windows users, you may have to include GHC's gcc-lib directory 
in your PATH environment, e.g., c:\ghc\ghc-6.8.3\gcc-lib, before 
configuring the GLFW module, otherwise it'll complain about missing
program for ld.

For Linux users there is an option to link to a system wide GLFW 
dynamical library instead of compilation from source. It can be
done by providing "--flags=dynamic" as an option to cabal configure
command. 

=============
Package Usage
=============

The package is tested with GHC 6.8.3 as well GHC 6.10.3 on all
three platforms (Linux, Win32/MinGW, and Mac OS X). Though it may
work with older versions of GHC or even Hugs, they are not tested.

It installs a new Haskell package called "GLFW" and the actual
module to import is "Graphics.UI.GLFW". You'll need to pass 
"-package GLFW" to GHC if you want to compile it.

GLFW itself is well documented (see GLFW website), and the
Haskell module API is documented via Haddock. 

Not all functions are fully tested, and there are still a 
few GLFW C functions missing from the Haskell module, namely 
the image loading functions. They are excluded because image
handling is a separate issue, and low level buffer manipulation
would obscure their use further. Texture loading from TGA
format is supported both from file and from memory (via a
string buffer). 

The Haskell module also provides basic text rendering while
GLFW doesn't. It comes from a free 8x16 font which is made
into a TGA texture, stored as a Haskell string in the file 
GLFW.hs (also the reason for its big size). Text rendering
is only possible with Alpha enabled. Again, see SOE.hs from
the SOE package for sample usage.

GLFW doesn't work well with GHC threads, forkIO or threadDelay. 
So avoid them if you can.


======================
Additional Information
======================

You may send your bug report and feature request to the package 
maintainer: Paul H. Liu <paul@thev.net>.

Lastest GLFW development is hosted in a darcs repository. You
may obtain it by

  darcs pull http://code.haskell.org/GLFW

There is also a mailinglist for GLFW deveopers at 

  http://projects.haskell.org/cgi-bin/mailman/listinfo/glfw

--
Last Updated: Wed Aug 12 EDT 2009

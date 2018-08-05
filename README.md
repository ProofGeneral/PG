# Proof General — Organize your proofs! 

[![Build Status](https://travis-ci.org/ProofGeneral/PG.svg?branch=master)](https://travis-ci.org/ProofGeneral/PG)

## Overview

Proof General is a generic Emacs interface for proof assistants.
The aim of the Proof General project is to provide a powerful, generic
environment for using interactive proof assistants.

This is version 4.5-git of Proof General.

## About Proof General branches

Two editions of Proof General are currently available:

* the (legacy) REPL-based, stable version of Proof General,
  gathered in the
  [master](https://github.com/ProofGeneral/PG/tree/master) branch, and
  licensed under GPLv2;
* the (newest) Coq-specific, experimental version of Proof General,
  supporting asynchronous proof processing,
  gathered in the
  [async](https://github.com/ProofGeneral/PG/tree/async) branch, and
  licensed under GPLv3+.

## Setup

Remove old versions of Proof General, then download and install the new release from GitHub:

```sh
git clone https://github.com/ProofGeneral/PG ~/.emacs.d/lisp/PG
cd ~/.emacs.d/lisp/PG
make
```

Then add the following to your `.emacs`:

```elisp
;; Open .v files with Proof General's Coq mode
(load "~/.emacs.d/lisp/PG/generic/proof-site")
```

If Proof General complains about a version mismatch, make sure that the shell's `emacs` is indeed your usual Emacs. If not, run the Makefile again with an explicit path to Emacs. On macOS in particular you'll probably need something like

```sh
make clean; make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
```

## More info

See:

* [INSTALL](INSTALL) for installation details
* [COPYING](COPYING) for license details
* [COMPATIBILITY](COMPATIBILITY) for version compatibility information
* [FAQ.md](FAQ.md) for frequently asked questions
* [coq/README](coq/README) for additional notes specific to the Coq prover

Links:

* [https://proofgeneral.github.io/doc](https://proofgeneral.github.io/doc) for online documentation of Proof General
* [http://proofgeneral.inf.ed.ac.uk/mailinglist](http://proofgeneral.inf.ed.ac.uk/mailinglist) for mailing list information

Supported proof assistants:

* Full support for latest versions of: [Coq](https://coq.inria.fr/)
* Support for previous versions of:
  [Isabelle](http://www.cl.cam.ac.uk/research/hvg/Isabelle/),
  [LEGO](http://www.dcs.ed.ac.uk/home/lego),
  [PhoX](http://www.lama.univ-savoie.fr/~RAFFALLI/phox.html)
* Experimental (less useful): CCC, ACL2, HOL98, Hol-Light, Lambda-Clam, Shell, Twelf
* Obsolete instances: Demoisa, Lambda-Clam, Plastic

A few example proofs are included in each prover subdirectory.

## Contributing

Contributions to this repository are placed under the BSD-3 license.
As BSD-3 is compatible with both GPLv2 and GPLv3+, this means that
we can merge them in both `master` and `async` branches if need be,
using the same license as the rest of the codebase, while you keep
all the rights on your code.
For more info, see <https://opensource.org/licenses/BSD-3-Clause>.

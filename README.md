# Proof General — Organize your proofs! 

[![CI](https://github.com/ProofGeneral/PG/workflows/CI/badge.svg?branch=master)](https://github.com/ProofGeneral/PG/actions?query=workflow%3ACI)
[![NonGNU ELPA](https://elpa.nongnu.org/nongnu/proof-general.svg)](https://elpa.nongnu.org/nongnu/proof-general.html)
[![MELPA](https://melpa.org/packages/proof-general-badge.svg)](https://melpa.org/#/proof-general)
[![MELPA Stable](https://stable.melpa.org/packages/proof-general-badge.svg)](https://stable.melpa.org/#/proof-general)  
[![ProofGeneral doc](https://img.shields.io/badge/doc-Proof%20General%20user%20manual-blue.svg)](https://proofgeneral.github.io/doc/master/userman/ "The ProofGeneral user manual")
[![PG-adapting doc](https://img.shields.io/badge/doc-PG--adapting-blue.svg)](https://proofgeneral.github.io/doc/master/adaptingman/ "PG's adapting manual to add provers")

## Overview

Proof General is a generic Emacs interface for proof assistants.
The aim of the Proof General project is to provide a powerful, generic
environment for using interactive proof assistants.

This is version 4.6-git of Proof General.

## About Proof General branches

Two editions of Proof General are currently available:

* the (standard) REPL-based, stable version of Proof General,
  gathered in the
  [master](https://github.com/ProofGeneral/PG/tree/master) branch;
* the (unmaintained) Coq-specific, experimental version of Proof General,
  supporting asynchronous proof processing,
  gathered in the
  [async](https://github.com/ProofGeneral/PG/tree/async) branch.

## Installing Proof General

Proof General requires GNU Emacs `25.2` or later.

The current policy aims at supporting multiple Emacs versions,
including those available in [Debian Stable](https://packages.debian.org/stable/emacs)
as well as in [Ubuntu LTS](https://packages.ubuntu.com/emacs) distributions
until their [End-Of-Support](https://wiki.ubuntu.com/Releases).

### Using NonGNU ELPA

[NonGNU ELPA](https://elpa.nongnu.org/) is the sister repository of
[GNU ELPA](https://elpa.gnu.org/) and enabled by default from Emacs 28
onwards.   You can directly install Proof General from NonGNU ELPA if
the repository is enabled.

### Using MELPA

[MELPA](https://melpa.org/) is a repository of Emacs packages. Skip
this step if you already use MELPA. Otherwise, add the following to
your `.emacs` and restart Emacs:

```elisp
(require 'package)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3") ; see remark below
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
```

**Remark:** If you have Emacs 26.1 (which is precisely
[the packaged version in Debian 10](https://packages.debian.org/emacs)),
you may get the error message `Failed to download 'melpa' archive`
during the package refresh step. This is a known bug
([debbug #34341](https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341))
which has been fixed in Emacs 26.3 and 27.1, while a simple workaround
consists in uncommenting the line
`(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")` above in your
`.emacs`.

**Note:** If you switch to MELPA from a previously manually-installed
Proof General, make sure you removed the old versions of Proof General
from your Emacs context (by removing from your `.emacs` the line
loading `PG/generic/proof-site`, or by uninstalling the proofgeneral
package provided by your OS package manager).

Then, run <kbd>M-x package-refresh-contents RET</kbd> followed by
<kbd>M-x package-install RET proof-general RET</kbd> to install and
byte-compile `proof-general`.

You can now open a Coq file (`.v`), an EasyCrypt file (`.ec`), a
qrhl-tool file (`.qrhl`), or a PhoX file (`.phx`) to automatically
load the corresponding major mode.

### Using Git (manual compilation procedure)

Remove old versions of Proof General, clone the PG repo from GitHub
and byte-compile the sources:

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

## Keeping Proof General up-to-date

### Using MELPA

As explained in the [MELPA documentation](https://melpa.org/#/getting-started), updating all MELPA packages in one go is as easy as typing
<kbd>M-x package-list-packages RET</kbd> then <kbd>r</kbd> (**r**efresh the package list), <kbd>U</kbd> (mark **U**pgradable packages), and <kbd>x</kbd> (e**x**ecute the installs and deletions).

### Using Git

Assuming you have cloned the repo in `~/.emacs.d/lisp/PG`, you would
have to run:

```sh
cd ~/.emacs.d/lisp/PG
make clean
git pull
make
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
* [https://coq.zulipchat.com](https://coq.zulipchat.com/) for chatting with PG maintainers and developers on the Zulip chat of Coq (in streams [Proof General devs](https://coq.zulipchat.com/#narrow/stream/304020-Proof-General.20devs) and [Proof General users](https://coq.zulipchat.com/#narrow/stream/304019-Proof-General.20users))
* [https://coq.gitlab.io/zulip-archive](https://coq.gitlab.io/zulip-archive) for the corresponding public Zulip archive (read-only, no authentication required)

Supported proof assistants:

* [Coq](https://coq.inria.fr/)
* [EasyCrypt](https://www.easycrypt.info/)
* [PhoX](https://raffalli.eu/phox/)
* [qrhl-tool](https://github.com/dominique-unruh/qrhl-tool/#readme)

Proof General used to support other proof assistants, but those
instances are no longer maintained nor available in the MELPA package:

* Experimental support of: Shell
* Obsolete instances: Demoisa
* Removed instances: Twelf, CCC, Hol-Light, ACL2, Plastic, Lambda-Clam, HOL98,
  [LEGO](http://www.dcs.ed.ac.uk/home/lego),
  [Isabelle](https://www.cl.cam.ac.uk/research/hvg/Isabelle/)

A few example proofs are included in each prover subdirectory.

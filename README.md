# Proof General — Organize your proofs! 

[![CI](https://github.com/ProofGeneral/PG/workflows/CI/badge.svg?branch=master)](https://github.com/ProofGeneral/PG/actions?query=workflow%3ACI)
[![MELPA](https://melpa.org/packages/proof-general-badge.svg)](https://melpa.org/#/proof-general)

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

## Installing Proof General

### Using MELPA (recommended procedure)

[MELPA](https://melpa.org/) is a repository of Emacs packages. Skip
this step if you already use MELPA. Otherwise, add the following to
your `.emacs` and restart Emacs:

```elisp
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)
```

**Note:** If you switch to MELPA from a previously manually-installed
Proof General, make sure you removed the old versions of Proof General
from your Emacs context (by removing from your `.emacs` the line
loading `PG/generic/proof-site`, or by uninstalling the proofgeneral
package provided by your OS package manager).

Then, run <kbd>M-x package-refresh-contents RET</kbd> followed by
<kbd>M-x package-install RET proof-general RET</kbd> to install and
byte-compile `proof-general`.

You can now open a Coq file (`.v`), an EasyCrypt file (`.ec`), or a
PhoX file (`.phx`) to automatically load the corresponding major mode.

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
* [http://proofgeneral.inf.ed.ac.uk/mailinglist](http://proofgeneral.inf.ed.ac.uk/mailinglist) for mailing list information

Supported proof assistants:

* [Coq](https://coq.inria.fr/)
* [EasyCrypt](https://www.easycrypt.info/)
* [PhoX](https://raffalli.eu/phox/)

Proof General used to support other proof assistants, but those
instances are no longer maintained nor available in the MELPA package:

* Legacy support of
  [Isabelle](https://www.cl.cam.ac.uk/research/hvg/Isabelle/) and
  [LEGO](http://www.dcs.ed.ac.uk/home/lego)
* Experimental support of: CCC, ACL2, HOL98, Hol-Light, Lambda-Clam, Shell, Twelf
* Obsolete instances: Demoisa, Lambda-Clam, Plastic

A few example proofs are included in each prover subdirectory.

## Contributing

Contributions to this repository are placed under the BSD-3 license.
As BSD-3 is compatible with both GPLv2 and GPLv3+, this means that
we can merge them in both `master` and `async` branches if need be,
using the same license as the rest of the codebase, while you keep
all the rights on your code.
For more info, see <https://opensource.org/licenses/BSD-3-Clause>.

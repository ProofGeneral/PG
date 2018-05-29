# Proof General — Organize your proofs!

[![Build Status](https://travis-ci.org/ProofGeneral/PG.svg?branch=async)](https://travis-ci.org/ProofGeneral/PG/branches)

## Overview

Proof General is an Emacs interface for the Coq proof assistant.

This is version 5.0-git of Proof General.

This version relies on Coq's XML protocol to support asynchronous
proof processing. That mechanism allows Coq to work on more than one
proof at a time, potentially speeding proof development.

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

Legacy versions of Proof General used a *Proof Shell*, essentially a
read-eval-print loop (REPL), to interact with Coq and other proof
assistants.  That very generic mechanism allowed Proof General to
support a number of other proof assistants. The more specialized XML
protocol allows Coq to send messages at any time, so that a REPL is
not an appropriate communication mechanism. Because the Proof Shell
has been removed, this version of Proof General supports only Coq.

## Setup

Remove any old versions of Proof General, then download and install the
new release from GitHub:

```
git clone https://github.com/ProofGeneral/PG
cd PG
git checkout async
make
```

Then add the following to your `.emacs`:

```
;; Open .v files with Proof General's Coq mode
(load "<path-to>/PG/generic/proof-site.el")
```

If Proof General complains about a version mismatch, make sure that
the shell's Emacs is indeed your usual Emacs. If not, run the Makefile
again with an explicit path to Emacs. On MacOS in particular you'll
probably need something like

```
make clean; make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
```

## Switching between Proof General versions

Please see the file HOWTO-USE for more detailed installation
instructions. That file explains how to switch between this version of
Proof General and the legacy, Proof Shell-based version.

## Contributing

Contributions to this repository are placed under the BSD-3 license.
As BSD-3 is compatible with both GPLv2 and GPLv3+, this means that
we can merge them in both `master` and `async` branches if need be,
using the same license as the rest of the codebase, while you keep
all the rights on your code.
For more info, see <https://opensource.org/licenses/BSD-3-Clause>.

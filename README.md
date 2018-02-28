# Proof General — Organize your proofs!

Proof General is an Emacs interface for Coq.
--------------------------------------------

This is version 5.0-git of Proof General, which relies on Coq's XML
protocol to support asynchronous proof processing. That mechanism
allows Coq to work on more than one proof at a time, potentially
speeding proof development.

Older versions of Proof General used a *Proof Shell*, essentially a
read-eval-print loop (REPL), to interact with Coq and other proof
assistants.  That very generic mechanism allowed Proof General to
support a number of other proof assistants. The more specialized XML
protocol allows Coq to send messages at any time, so that a REPL is
not an appropriate communication mechanism. Because the Proof Shell
has been removed, this version of Proof General supports only Coq.

Setup
-----

Remove any old versions of Proof General, then download and install the
new release from GitHub:

```
git clone https://github.com/ProofGeneral/PG
cd PG
git checkout async
make
```

Then add the following to your .emacs:

```
;; Open .v files with Proof General's Coq mode
(load "<path-to>/PG/generic/proof-site")
```

If Proof General complains about a version mismatch, make sure that
the shell's Emacs is indeed your usual Emacs. If not, run the Makefile
again with an explicit path to Emacs. On MacOS in particular you'll
probably need something like

```
make clean; make EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
```

Switching between Proof General versions
----------------------------------------

Please see the file HOWTO-USE for more detailed installation
instructions. That file explains how to switch between this version of
Proof General and the legacy, Proof Shell-based version.

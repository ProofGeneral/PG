FAQs for using/installing Proof General
=======================================

With thanks to the anonymous authors of questions/answers below.

For latest version, see https://github.com/ProofGeneral/PG/blob/master/FAQ.md
Please also check the BUGS file.

-----------------------------------------------------------------

Q. I use ProofGeneral with custom installations of Coq, depending
   on the project I am working on. How can I change `coq-prog-name`
   accordingly?

A. The recommended way to set `coq-prog-name` is to create a file
   .dir-locals.el in the top-level folder of your Coq project (or
   if applicable, in the sub-folder containing the Coq source files)
   with content:

    ((coq-mode . ((coq-prog-name . ".../path/to/coqtop"))))

   Then restart Emacs
   (or just run: `M-x proof-shell-exit RET yes RET`, `M-x normal-mode RET`
   in the Coq buffer before restarting the Coq process) in order
   to take this change into account.

-----------------------------------------------------------------

Q. Coq produces some useful output I'd like to keep a note of, how do I do that?

A. Some people cut and paste into comments in their source files.
   But you can easily make new files or temporary buffers in Emacs:

   * copy text from \*response* or \*goals* buffer
   * `C-x b <enter new name> RET`
   * Switch to correct mode: `M-x coq-mode RET`
   * Paste text, the highlighting/sumbols should appear correctly.

-----------------------------------------------------------------

Q. Proof General fails to load with an error message on start-up,
   containing text like this:

    Proof General was compiled for GNU Emacs 23.1 but
    is running on Emacs 22.3: please run "make clean; make"

   What's wrong?

A. We distribute compiled .elcs for one version of Emacs, but other
   versions use different bytecode formats.  You will have to delete
   the compiled files and (optionally) recompile for your preferred
   Emacs version.  Using the Makefile:

    make clean  # removes all .elc files.

   and then a command like this:

    make EMACS=emacs-22.3

   (without the EMACS setting just uses 'emacs').

-----------------------------------------------------------------

Q. I have just installed Emacs, ProofGeneral and a proof assistant.
    It works but Tokens (e.g. \\&lt;Longrightarrow&gt;) are not being displayed
    as symbols.

A. You need to enable Unicode Tokens by the menu item:

    Proof-General -> Options -> Unicode Tokens

   To enable it automatically every time you use Proof General,
   use

    Proof-General -> Options -> Save Options

   after doing this.

   Note that we don't do this by default, because from the system's
   perspective it is difficult to determine if this will succeed ---
   or just produce funny characters that confuse new users even more.

Q. With Unicode symbols enabled, the symbols look a mess, e.g.
   compressed and overlap one another.

A. Unfortunately this is a bug in the display engine inside
   certain versions of Emacs, for example the default version
   of emacs, Emacs 23.3.1 on Ubuntu 11.10, suffers.

   The solution is to switch to another version (e.g. Emacs 23.2).
   (See Trac#409: <http://proofgeneral.inf.ed.ac.uk/trac/ticket/409>)

   You may be able to get better results with different fonts, even
   without upgrading Emacs.

   Proof General uses Deja Vu Sans Mono by default because
   this often works out-of-the-box.  But STIX is better if you
   install it.  See <http://www.stixfonts.org/>.   On Ubuntu try:

    sudo apt-get install fonts-stix

   To change to STIX, either

    M-x customize face RET unicode-tokens-symbol-font-face RET

   and edit to set the family name to "STIXGeneral", or edit
   the line beginning `(defface unicode-tokens-symbol-font-face` in
   lib/unicode-tokens.el.

-----------------------------------------------------------------

Q. Help, I'm stuck!! Emacs keeps telling me "Cannot switch buffers in a
   dedicated window"

A. This can happen if you enabled "Use Three Panes" and then change
   the panes (window) layout manually, typically by deleting another
   window or frame so you only have a "dedicated" window on the
   display.  Don't kill Emacs!  There are many ways of getting out,
   e.g.

   * In single window mode, `C-c C-l` (proof-layout-windows) refreshes
     the display

   * In multiple window mode, if you have accidently deleted the main
     window, get a new one with `M-x new-frame RET`

-----------------------------------------------------------------

Q. I have a problem installing/using Proof General, what can I do?

A. Please check the documentation carefully, particularly the
   requirements for a full-featured and recent Emacs version, as
   mentioned in [INSTALL](INSTALL) (see "Dependency on Other Emacs Packages").  
   If you still cannot solve your problem, try to contact someone
   else who is using Proof General with a similar setup.  The
   best way to do this may be through the user mailing list for your
   proof assistant.  If you think the problem is Proof General related,
   consult the PG Wiki and Trac pages.

-----------------------------------------------------------------

Q. I'm using Proof General for prover X, then I load a file for
   prover Y.  I get an error.  Why?

A. Unfortunately the architecture of Proof General is designed so
   that you can only use one prover at a time in the same Emacs
   session.   If you want to run more than one prover at a time,
   you have to run more than one Emacs.

-----------------------------------------------------------------

Q. I'm afraid I got stuck very early on.  I sent the following line:

    by (swap_res_tac [psubsetI] 1;

   Notice that I forgot the right bracket.  The line went pink, the
   buffer went read-only and nothing I tried would let me fix the
   error.

A. The proof process is waiting for more input because of the missing
   parenthesis, but Proof General doesn't realise this and waits for a
   response.  You should type something in the proof shell buffer
   (\*isabelle*), or interrupt the process with `C-c C-c` or the Stop button.

-----------------------------------------------------------------

Q. How can I keep the Proof General option settings across sessions?

A. For options set in the `Proof General -> Options` menu use the
   "Save Options" menu item (`Proof General -> Options -> Save Options`).

   For other options set via customize
   (`Proof General -> Advanced -> Customize`), use the customize buttons,
   or `M-x customize-save-customized`.

-----------------------------------------------------------------

Q. The "Favourites" feature to insert/send fixed strings is great,
   but I'd like to define a command which takes arguments.

A. You can do that in Elisp with a command like this:

    (proof-definvisible isar-theorem
      '(format "thm %s" (read-string "theorem: "))
       [(control t)])

   (NB: it binds the key `C-c C-a C-t`).  See the documentation for
   `proof-definvisible` and `proof-defshortcut`.

-----------------------------------------------------------------

Q. Why do I get a warning "Bad version of xml.el found, ..."?

A. Your Emacs distribution includes a version of xml.el which has
   fundamental bugs.   The patched version of xml.el, in lib/xml-fixed.el
   has been loaded instead.  This works for Proof General because it fixes
   the basic bugs, but it may cause compatibility issues in other packages
   (e.g. it is quite different from the latest xml.el with GNU Emacs
   development versions).

   This message is probably nothing to worry about unless you are using
   the same Emacs session for other packages that heavily use xml.el
   (e.g. GNUS).

-----------------------------------------------------------------

Q. Can I join any mailing lists for Proof General?

A. Yes, there are two mailing lists available, one for users, and another
for those interested in developing Proof General. Visit this Web site
for full details:

    http://proofgeneral.inf.ed.ac.uk/mailinglist

Q. Emacs appears to hang when the prover process is started.

A. One thing is to check the variable `comint-process-echoes` which
   might be non-nil for the \*coq* (or other prover) buffer.  It
   should be nil.

   The default value of comint-process-echoes is nil.  Move any
   modifications of this variable away from the top level (e.g.,
   .emacs file, which affects the \*coq*-buffer), and down to the
   mode-hooks which require them (e.g. shell-mode-hook).  The
   variable might also have been set by Customize, it can be reset
   with `M-x customize-variable RET comint-process-echoes RET`.

   A reason with older versions of Isabelle and Coq (before 2007) was
   the emergence of UTF-8 support in linuxes with Glibc 2.2 and
   later, enabled with UTF8 encoded output in your default locale.
   Proof General used on 8-bit characters which are UTF8 prefixes in
   the output of proof assistants.  These prefix characters were not
   flushed to stdout individually.

   As a workaround we can disable interpretation of UTF8 in the C
   libraries.  Doing this inside Proof General is unreliable; locale
   settings are set/inherited in strange ways.  One solution is to
   run the Emacs process itself with an altered locale setting, e.g.,

    $ LC_CTYPE=en_GB xemacs &

   (where $ is the shell prompt; this example is for my locale which
   by default is "en_GB.UTF-8": I see this by typing "locale" at
   the prompt).

   (This fix is attempted in the supplied "proofgeneral" script, as
   well as making an adjustment in Proof General when the string UTF
   appears in the current value of LC\_CTYPE.  Alternatively you can
   set LC\_CTYPE inside a file ~/.i18n, which will be read the shell.
   Put a line such as "LC\_CTYPE=en_GB" into this file.  However, this
   action will affect all applications.

   NB: a related issue is warnings from X-Symbol: "Emacs language
   environment and system locale specify different encoding, I'll
   assume `iso-8859-1`".  This warning appears to be mostly harmless.
   Notice that the variable `buffer-file-coding-system` may determine
   the format that files are saved in.

   Another way to affect this which has been suggested is to add a line
   like this to the init.el file on XEmacs:

    (prefer-coding-system 'ctext)

   but I haven't tried this.

   The above fixes should not be necessary with most recent prover
   versions.  Isabelle 2007 has a "Unicode-safe" interaction mode,
   enabled by default (to disable, customize `proof-shell-unicode`).
   This is also used by the Isabelle startup scripts.  Coq 8.1 and
   later do not use non-ASCII characters in output.

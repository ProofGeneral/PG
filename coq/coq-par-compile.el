;;; coq-par-compile.el --- parallel compilation of required modules  -*- lexical-binding: t; -*-

;; This file is part of Proof General.

;; Portions © Copyright 1994-2012  David Aspinall and University of Edinburgh
;; Portions © Copyright 2003-2021  Free Software Foundation, Inc.
;; Portions © Copyright 2001-2017  Pierre Courtieu
;; Portions © Copyright 2010, 2016  Erik Martin-Dorel
;; Portions © Copyright 2011-2013, 2016-2017, 2019-2021 Hendrik Tews
;; Portions © Copyright 2015-2017  Clément Pit-Claudel

;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; This file implements compilation of required modules.  The
;; compilation is done in parallel in the background (in contrast to
;; what you find in coq-seq-compile.el).
;;
;;
;;; TODO
;;
;; - fix -I current-dir argument for coqc invocations
;; - on error, try to put location info into the error message
;; - use file-attribute-modification-time and similar functions when
;;   dropping support for emacs 25 (emacs 26.1 released on 05/2018)
;; - use define-error when dropping support for emacs 24 (25.1
;;   released on 09/2016)
;;
;; Note that all argument computations inherit `coq-autodetected-version': when
;; changing compilers, all compilation jobs must be terminated.  This is
;; consistent with the fact that the _CoqProject file is not reparsed.

;;; Code:

(defvar queueitems)       ; dynamic scope in p-s-extend-queue-hook
(eval-when-compile (require 'cl-lib))
(require 'coq-compile-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Multiple file handling -- parallel compilation of required modules
;;

;; This file implements parallel background compilation. It makes sure
;; that only a certain number (`coq-max-background-compilation-jobs')
;; of coqdep and coqc processes are running in the background.
;;
;; In this file, compilation jobs are uninterned lisp symbols that
;; store all important information in their property list. New
;; compilation jobs are created when Require commands are recognized and
;; when the output of coqdep is processed. If there is space, new jobs
;; are directly launched. Otherwise, they are put into a queue
;; (`coq--par-compilation-queue') to be launched when some other
;; process terminates.
;;
;; Dependencies between files are reflected with suitable links. They
;; are established when the coqdep output is processed. A job with
;; dependencies waits for the dependencies to finish before it
;; continues with coqc.
;;
;; There are two differend kinds of compilation jobs. First, ordinary
;; files, whose dependencies must be determined with coqdep and which
;; might need to be compiled after all their dependencies are ready.
;; These compilation jobs are called 'file jobs here. Apart from 'file
;; jobs there are the Require commands in the asserted region. For
;; each Require command one must determine the modules/files (its
;; dependencies) that must be compiled before the Require command can
;; be processed. Require commands are tracked with 'require jobs here.
;; Typically, a 'require job has a number of 'file jobs as
;; dependencies. coqdep must be run on both, 'require and 'file jobs
;; to determine their dependencies. For 'require jobs this is done in
;; a temporary file. coqc must only be run on 'file jobs.
;;
;; It is pretty clear how to process these compilation jobs. The
;; problems are:
;;
;; 1- where to put the Require command itself and the items that follow it
;; 2- make sure ancestors are properly locked
;; 3- error reporting
;; 4- using -quick and the handling of .vo/.vio prerequisites for Coq < 8.11
;; 5- using -vos for Coq >= 8.11
;; 6- running vio2vo or -vok to check proofs
;; 7- default-directory / current directory and buffer local variables
;;
;;
;; For 1- where to put the Require command and the items that follow it:
;;
;; The Require command and the items that follow cannot stay in
;; proof-action-list, as otherwise they would be sent to the prover
;; long before the compilation finishes. I therefore cut
;; proof-action-list into pieces and leave only the items before the
;; first Require on proof-action-list. The others are put into the
;; 'queueitems property of the 'require command that is created for
;; the Require. When such a 'require job gets ready, it puts the items
;; back into proof-action-list and lets Proof General process them as
;; usual.
;;
;; All 'require commands are linked with so-called 'queue-dependant
;; links, such that later 'require jobs can be delayed until earlier
;; ones are ready. The later 'require job is said to be a queue
;; dependant of the earlier one.
;;
;;
;; For 2- make sure ancestors are properly locked:
;;
;; Consider "Require a. Require b." where a and b depend on c. Locking
;; must be done such that c is only unlocked, when "Require a" is
;; undone and not when "Require b" is undone alone. In case
;; compilation of c fails then c and all its ancestors must be
;; unlocked. During compilation, files are locked just before coqdep
;; is started on them (after they have been identified as a
;; dependency). At that time the 'lock-state property of the job is
;; set to 'locked. When a require job is retired, all ancestors with
;; 'lock-state property 'locked are collected by following the
;; downward links in a depth-first recursion. (Previous versions that
;; collected ancestors upwards during dependant kickoff suffered from
;; exponential blowup, see issues #499 and #572.) When the require job
;; was successful, the collected jobs are stored in the
;; 'coq-locked-ancestors property of the span belonging to that
;; require command (in the 'require-span property). Furhter, the
;; 'lock-state is set to 'asserted, such that another collection from
;; a following require job ignores these jobs. A span delete action
;; will unlock all uncestors in the 'coq-locked-ancestors property.
;;
;; When the require job was unsuccessful, all collected jobs are
;; unlocked.
;;
;;
;; For 3- error reporting:
;;
;; Depending on `coq-compile-keep-going' compilation can continue
;; after an error or stop immediately. For stopping immediately,
;; processing is aborted with a signal that eventually leads to
;; `coq-par-emergency-cleanup', which kills all compilation jobs,
;; retracts the queue region and resets all internal data.
;;
;; For `coq-compile-keep-going', the failing job, all ordinary
;; dependants and all queue dependants are marked with 'failed.
;; Therefore, if any job failed, the last require job is also marked
;; as failed. Ancestors of failing require jobs are unlocked only when
;; this last require is retired. At that time any ancestors of any
;; preceeding successful require jobs have already been asserted.
;; Failed jobs continue with their normal state transition, but omit
;; certain steps (eg., running coqc). If the last compilation job is
;; marked as failed at the time it is retired, then the whole queue
;; region is retracted and all ancestors of this and all preceeding
;; failed require jobs are unlocked. The ancestors to unlock are those
;; with 'lock-state 'asserted and they are collected from the
;; dependency tree just before unlocking. (Previous versions that
;; collected ancestors upwards during dependant kickoff suffered from
;; exponential blowup, see issues #499 and #572.)
;;
;; When a failing require command follows a bunch of commands that
;; take a while to process, it may happen, that the last failing
;; require command is ready to be retired before the preceeding
;; commands have been processed. In this case the retirement (in
;; particular, unlocking the queue region) must be delayed until proof
;; action list is empty. This is done by adding an empty action into
;; `proof-action-list' that calls the retirement function
;; `coq-par-kickoff-queue-maybe' again. Further
;; `coq--par-delayed-last-job' must be set to disable the cycle
;; detection that is otherwise automatically started if no background
;; job is active and the last require job has not been retired yet.
;;
;;
;; For 4- using -quick and the handling of .vo/.vio prerequisites for Coq < 8.11
;;
;; There are now two ways available to speed up compilation,
;; -quick/-vio and -vos. For Coq >= 8.5 and < 8.11 -quick/-vio is
;; available and coq-compile-quick is consulted in order to determine
;; whether to use it. For Coq >= 8.11 -vos is available and
;; coq-compile-vos is consulted. The following paragraph descripes the
;; complications with -quick/-vio.

;; Coq accepts both .vo and .vio files for importing modules
;; regardless of it is running with -quick or not. However, it is
;; unclear which file is loaded when both, .vo and .vio, of a
;; dependency are present. Therefore I delete a .vio file when I
;; decide to rebuild a .vo file and vica versa. coqdep delivers
;; dependencies for both, .vio and .vo files. These dependencies are
;; identical for .vio and vo (last checked for coq trunk in October
;; 2016). For deciding whether prerequisites must be recompiled the
;; full path returned form coqdep is relevant. Because it seems odd to
;; store a full path without a .vo or .vio suffix I decided to always
;; store the .vo object file name in the 'vo-file property of
;; compilation jobs. Only when all dependencies are ready, in
;; `coq-par-job-needs-compilation' I decide whether to build a .vio or
;; .vo file and if already present .vo or .vio files must be deleted.
;; Only at that point the relevant property 'required-obj-file is set.
;;
;;
;; For 5- using -vos for Coq >= 8.11
;;
;; When Coq >= 8.11 is detected, -vos is used and coq-compile-vos is
;; consulted in order to find out whether to compile to .vos or to .vo
;; files. For people switching PG or switching to Coq 8.11, there is a
;; backward compatibility path: When coq-compile-vos is nil, the
;; decision is derived from coq-compile-quick.
;;
;; The logic here to decide whether compilation is needed or not,
;; assumes coq is used consistently without randomly deleting files.
;; Because coqc always produces an empty .vos file when compiling to
;; .vo, it can never happen, that a .vo file is present without .vos
;; file or that a .vo file is more recent then the corresponding .vos
;; file. This simplifies the logic for deciding about recompilation
;; quite a bit.
;;
;;
;; For 6- running vio2vo or -vok to check proofs
;;
;; To check proofs when compilation is done with -quick/-vio or -vok,
;; a second stage can be configured using 'quick-and-vio2vo and
;; 'vos-and-vok. If configured, coq-par-job-needs-compilation sets the
;; 'second-stage property if the job needs a second stage run to check
;; the proofs. Such jobs are stored in `coq--par-second-stage-queue'
;; until first stage compilation and library loading has been done and
;; `coq-compile-second-stage-delay' is over. To ensure the timer
;; starts after loading libraries, it is started in a callback from an
;; otherwise empty action list item when the first stage compilation
;; produced no error. When there was an error, the timer is started
;; directly when retiring the last require job. Jobs that participate
;; in the second stage are in state 'ready. They do not change their
;; state in the second stage.
;;
;; The user can relatively easily achieve that there is more than one
;; action item with a callback to start the delay timer present in
;; `proof-action-list'. To ensure only the last one starts the timer,
;; the callback remembers the value of
;; `coq--par-second-stage-start-id' and only acts if that is still the
;; current value when the callback is executed.
;;
;;
;; For 7- default-directory / current directory and buffer local variables
;;
;; Sentinels and timer functions inherit local variables and the
;; current directory (`default-directory') from the basically random
;; buffer that is current when they are invoked. Users may have
;; configured `coq-compiler' or other variables that influence
;; background compilation, see issue #797. Therefore all entry points
;; of this library must temporarily switch to the scripting buffer
;; that caused the compilation, thereby implicitely also setting
;; `default-directory' to the correct value. Such entry points are
;; coq-par-process-sentinel, the functions started from timer and
;; those started from an empty entry in `proof-action-list'. To set
;; the current buffer in these cases, I record it in 'script-buf inside
;; `coq-par-handle-require-list' and then pass it on to all jobs
;; created.
;; 
;; 
;; Properties of compilation jobs
;;
;;   'name            - some unique string, only used for debugging
;;   'queueitems      - holds items from proof-action-list on
;;                      require jobs
;;   'vo-file         - the .vo file for the module that this job has
;;                      to make up-to-date. This slot is filled when the
;;                      job is created and independent of whether a .vio
;;                      or .vo file must be made up-to-date. Only present
;;                      in file jobs.
;;   'required-obj-file - contains the .vio or .vo to be produced or nil
;;                        if that has not yet been decided. May contain
;;                        nil if no file needs to be rebuild at all. Nil
;;                        on require jobs.
;;   'obj-mod-time    - modification time of 'required-obj-file, stored
;;                      here in case compilation is not needed for file
;;                      jobs, to avoid double stat calls;
;;   'use-quick       - 'vio if `coq-par-job-needs-compilation' decided to use
;;                      -quick for Coq older than 8.11; 'vos if it decided to
;;                      to use -vos for Coq 8.11 or newer, nil if it decided
;;                      to use .vo compilation, no compilation is necessary or
;;                      for require jobs.
;;   'type            - the type of the job, either 'require or 'file
;;   'state           - the state of the job, see below
;;   'coqc-dependants - list of parent jobs that depend on this job,
;;                      when this job finishes it propagates the
;;                      necessary information to it's parent jobs and
;;                      decreases their 'coqc-dependency-count
;;   'coqc-dependees  - list of child jobs this job depends on
;;                      reverse links of 'coqc-dependants, needed to
;;                      efficiently traverse the tree to collect ancestors
;;                      for locking and unlocking
;;   'coqc-dependency-count - number of unfinished child jobs
;;                            increased for every subjob spawned
;;                            during coqdep output processing
;;                            File job waits with coqc until this
;;                            count reaches 0 again. Require jobs wait with
;;                            their transition to 'ready.
;;   'youngest-coqc-dependency - slot to accumulate the most recent
;;                               modification time of some ancestor
;;                               value; might be an Emacs time or
;;                               'just-compiled; not really needed in require
;;                               jobs but present there to simplify the code
;;   'queue-dependant - next top-level job, only present in require jobs
;;   'queue-dependant-waiting - t if this is a require job that has
;;                              to wait until previous require jobs
;;                              finish. In this waiting time, dependencies
;;                              are compiled, but queue items are only
;;                              put back into proof-action-list when
;;                              this property becomes nil
;;   'queue-dependee  - previous top-level job, only present in require jobs
;;                      reverse link of 'queue-dependant, needed to unlock
;;                      ancestors in case of error
;;   'src-file        - the .v file name, only in file jobs
;;   'load-path       - value of coq-load-path, propagated to all
;;                      dependencies
;;   'lock-state      - nil for require jobs, 'unlocked if the file
;;                      corresponding to job is not locked, 'locked if that
;;                      file has been locked, 'asserted if it has been
;;                      registered in some span in the 'coq-locked-ancestors
;;                      property already
;;   'require-span    - holds the span with the require command for require jobs
;;   'second-stage    - nil if no second stage is required, otherwise 'vio2vo
;;                      for <8.11 or 'vok for >=8.11; set in
;;                      coq-par-job-needs-compilation
;;                      build the .vo file. Otherwiese nil.
;;   'failed          - t if coqdep or coqc for the job or one dependee failed.
;;   'visited         - used in the dependency cycle detection to mark
;;                      visited jobs
;;   'script-buf      - Buffer that cause the background compilation, i.e.,
;;                      that contained a require command. This buffer
;;                      is propagate to all dependencies and used as
;;                      current buffer in all asynchronous functions
;;                      (sentinels, timer functions, etc). This
;;                      ensures that local variables and
;;                      `default-directory' have correct values.
;;   'temp-require-file  - temporary file name just containing the require
;;                         command of a require job for determining the files
;;                         needed for that require. Must be deleted after
;;                         coqdep finished.
;;
;; 
;; Properties of processes
;;
;; A lot of the necessary actions are started from inside a process
;; sentinel. The property list of processes stores the necessary
;; information for that in the following properties.
;;
;;   'coq-compilation-job       - the compilation job that started
;;                                this process
;;   'coq-process-continuation  - the continuation to be called when
;;                                the process finishes. Either
;;                                coq-par-process-coqdep-result or
;;                                coq-par-coqc-continuation or
;;                                coq-par-vio2vo-continuation
;;   'coq-process-output        - the output of the process
;;   'coq-process-command       - the command for error reporting
;;                                (as string list)
;;   'coq-par-process-killed    - t if this process has been killed from PG
;;   'coq-process-rm            - if not nil, a file to be deleted when
;;                                the process does not finish successfully,
;;                                i.e., when the continuation is not called.
;;
;;
;; State transition of file jobs
;;
;;     'enqueued-coqdep -> 'waiting-dep -> 'enqueued-coqc -> 'ready
;;
;; State transition for require jobs
;;
;;     'enqueued-coqdep -> 'waiting-dep -> 'waiting-queue -> 'ready
;;
;; State explanation
;;
;;   'enqueued-coqdep - coqdep is running or the job enqueued, waiting
;;                      for a slot to start coqdep
;;   'waiting-dep     - coqdep finished, dependencies determined, waiting for
;;                      the dependencies
;;   'enqueued-coqc   - dependencies ready, coqc is running, or the job is
;;                      enqueued, waiting for a slot to start coqc
;;   'waiting-queue   - dependencies ready, waiting for queue dependee
;;                      to get ready
;;   'ready           - ready, the result might be missing when 'failed
;;
;;
;; Over its live time, a compilation job might get passed through the
;; following functions. Some of them might be skipped or executed more
;; then once.
;;
;; | functions for file jobs          | state            | comment                              |
;; |----------------------------------+------------------+--------------------------------------|
;; | coq-par-create-file-job          | 'enqueued-coqdep | job creation                         |
;; | coq-par-start-coqdep-on-file     |                  | lock files                           |
;; | coq-par-process-coqdep-result    | 'waiting-dep     | create dependee/child jobs           |
;; | coq-par-decrease-coqc-dependency |                  | dependee/child finished coqc         |
;; | coq-par-compile-job-maybe        | 'enqueued-coqc   |                                      |
;; | -- start coqc                    |                  |                                      |
;; | coq-par-coqc-continuation        |                  |                                      |
;; | coq-par-kickoff-coqc-dependants  | 'ready           |                                      |
;; | coq-par-start-vio2vo             |                  |                                      |
;; | coq-par-vio2vo-continuation      |                  |                                      |
;;
;;
;; | function for require jobs        | state            | comment                              |
;; |----------------------------------+------------------+--------------------------------------|
;; | coq-par-create-require-job       | 'enqueued-coqdep | job creation                         |
;; | coq-par-start-coqdep-on-require  |                  | lock files                           |
;; | coq-par-process-coqdep-result    | 'waiting-dep     | create dependee/child jobs           |
;; | coq-par-decrease-coqc-dependency |                  | dependee/child finished coqc         |
;; | coq-par-kickoff-queue-maybe      | 'waiting-queue   |                                      |
;; | coq-par-retire-top-level-job     |                  |                                      |
;; | coq-par-kickoff-queue-maybe cont | 'ready           |                                      |
;; | coq-par-require-processed        |                  | in dummy action in proof-action-list |
;; | coq-par-run-second-stage-queue   |                  | called via timer                     |
;;
;;
;; The following _is_ outdated.
;; To print the states of the compilation jobs for debugging, eval
;; 
;; (let ((clones))
;;   (maphash (lambda (k v)
;; 	     (message "%s type %s for %s state %s dep of %s queue dep of %s"
;; 		      (get v 'name)
;; 		      (get v 'type)
;; 		      (get v 'src-file)
;; 		      (get v 'state)
;; 		      (mapcar (lambda (p) (get p 'name))
;; 			      (get v 'coqc-dependants))
;; 		      (get v 'queue-dependant))
;; 	     (mapc (lambda (p) (when (eq (get p 'type) 'clone)
;; 				 (push p clones)))
;; 		   (get v 'coqc-dependants)))
;; 	   coq--compilation-object-hash)
;;   (mapc (lambda (v)
;; 	  (message "%s type %s for %s state %s dep of %s queue dep of %s"
;; 		   (get v 'name)
;; 		   (get v 'type)
;; 		   (get v 'src-file)
;; 		   (get v 'state)
;; 		   (mapcar (lambda (p) (get p 'name))
;; 			   (get v 'coqc-dependants))
;; 		   (get v 'queue-dependant)))
;; 	clones))


;;; Variables

(defvar coq--current-background-jobs 0
  "Number of currently running background jobs.")

(defvar coq--compilation-object-hash nil
  "Hash for storing the compilation jobs.
This hash only stores file jobs and no require jobs.  They
are stored in order to avoid double compilation.  The jobs stored
in here are uninterned symbols that carry all important
information in their property list.  See the documentation in the
source file \"coq-par-compile.el\". The hash always maps .vo file
names to compilation jobs, regardless of ``-quick''.")

(defvar coq--last-compilation-job nil
  "Pointer to the last require compilation job.
Used to link top-level jobs with queue dependencies.")

(defvar coq--par-second-stage-in-progress nil
  "t iff vio2vo or vok is running in background.")

(defvar coq--par-second-stage-delay-timer nil
  "Holds the timer for the second stage delay.")

(defvar coq--par-second-stage-start-id 0
  "Integer counter to detect races for `coq-par-require-processed'.
Assume the last top-level ``Require'' command is retired, but
executing the ``Require'' takes so long that the user can assert
a next ``Require'' and that the second require is retired before
the first ``Require'' has been processed. In this case there are
two `coq-par-require-processed' callbacks active, of which the
first one must be ignored. For each new callback this counter is
incremented and, when there is a difference,
`coq-par-require-processed' does not start the second stage
timer. In case the first callback gets executed before the second
require is retired, the race is detected with
`coq--last-compilation-job' without this counter.")

(defvar coq--par-next-id 1
  "Increased for every job and process, to get unique job names.
The names are only used for debugging.")

(defvar coq--par-delayed-last-job nil
  "Inform the cycle detection that there is a delayed top-level job.
The retirement of failing require jobs must be delayed until
`proof-action-list' is empty, to avoid retracting commands before
the first failing require. This is done by adding the call to
`coq-par-kickoff-queue-maybe' to the end of `proof-action-list'.
This variable is t during this time to disable cycle detection,
which is otherwise started when `coq--last-compilation-job' has
not been retired but no compilation process is running in the
background.")


;;; utility functions

(defun split-list-at-predicate (l pred)
  "Split L into several lists at points where PRED is t.
Splits L into several lists, such that
- their concatenation equals the original L
- every element for which PRED returns t starts a new list
- except for the first list, PRED is t for every car of every result list
- the first result list contains the first elements of L for which PRED is nil
L is modified in place and the list structure in L is reused for
the result."
  (let ((result (list l))
	previous
	(current l))
    (while current
      (when (funcall pred (car current))
	(if previous
	    (progn
	      (setcdr previous nil)
	      (push current result))
	  ;; handle case where pred is t for the car of the original l
	  (setq result (list l nil))))
      (setq previous current)
      (setq current (cdr current)))
    (nreverse result)))

(defun coq-par-time-less (time-1 time-2)
  "Compare extended times.
The arguments can be an emacs time (a list of 2 to 4 integers,
see `current-time') or the symbol 'just-compiled, where the
latter is greater then everything else."
  (cond
   ((eq time-2 'just-compiled) t)
   ((eq time-1 'just-compiled) nil)
   (t (time-less-p time-1 time-2))))

(defun coq-par-init-compilation-hash ()
  "(Re-)Initialize `coq--compilation-object-hash'."
  (setq coq--compilation-object-hash (make-hash-table :test 'equal)))


;;; generic queues
;; Standard implementation with two lists.

(defun coq-par-new-queue ()
  "Create a new empty queue."
  (cons nil nil))

(defun coq-par-enqueue (queue x)
  "Insert X in queue QUEUE."
  (push x (car queue)))

(defun coq-par-queue-reverse-if-needed (queue)
  "Reorganize QUEUE, such that the pop-end contains something.
Of course only if necessary and possible."
  (unless (cdr queue)
    (setcdr queue (nreverse (car queue)))
    (setcar queue nil)))

(defun coq-par-queue-head (queue)
  "Return the head of QUEUE, if any, without removing it."
  (coq-par-queue-reverse-if-needed queue)
  (cadr queue))

(defun coq-par-dequeue (queue)
  "Dequeue the next item from QUEUE."
  (coq-par-queue-reverse-if-needed queue)
  (pop (cdr queue)))

(defun coq-par-queue-length (queue)
  "Length of QUEUE."
  (+ (length (car queue)) (length (cdr queue))))


;;; job queue

(defvar coq--par-compilation-queue (coq-par-new-queue)
  "Queue of compilation jobs that wait for a free core to get started.
Use `coq-par-job-enqueue' and `coq-par-job-dequeue' to access the
queue.")

(defun coq-par-job-enqueue (job)
  "Insert JOB in the queue of waiting compilation jobs."
  (coq-par-enqueue coq--par-compilation-queue job)
  (when coq--debug-auto-compilation
    (message "%s: enqueue job in waiting queue" (get job 'name))))

(defun coq-par-job-dequeue ()
  "Dequeue the next job from the compilation queue."
  (let ((res (coq-par-dequeue coq--par-compilation-queue)))
    (when coq--debug-auto-compilation
      (if res
	  (message "%s: dequeue" (get res 'name))
	(message "compilation queue empty")))
    res))


;;; queue for second stage (vio2vo or vok)

(defvar coq--par-second-stage-queue (coq-par-new-queue)
  "Queue of jobs that need a vio2vo or vok process.
Use `coq-par-second-stage-enqueue',
`coq-par-second-stage-dequeue' and
`coq-par-second-stage-queue-length' to access the queue.")

(defun coq-par-second-stage-enqueue (job)
  "Insert JOB in the queue for second stage processing."
  (coq-par-enqueue coq--par-second-stage-queue job)
  (when coq--debug-auto-compilation
    (message "%s: enqueue job in second stage queue" (get job 'name))))

(defun coq-par-second-stage-head ()
  "Return the head of the second stage queue, if any, without removing it."
  (coq-par-queue-head coq--par-second-stage-queue))

(defun coq-par-second-stage-dequeue ()
  "Dequeue the next job from the second stage queue."
  (let ((res (coq-par-dequeue coq--par-second-stage-queue)))
    (when coq--debug-auto-compilation
      (if res
	  (message "%s: second stage dequeue" (get res 'name))
	(message "second stage queue empty")))
    res))

(defun coq-par-second-stage-queue-length ()
  "Return the length of the second stage queue."
  (coq-par-queue-length coq--par-second-stage-queue))


;;; error symbols

;; coq-compile-error-coqdep
;;
;; This error is signaled with one data item -- the file name

(put 'coq-compile-error-coqdep 'error-conditions
     '(error coq-compile-error coq-compile-error-coqdep))
(put 'coq-compile-error-coqdep 'error-message
     "Coq compilation error: coqdep fails in")

;; coq-compile-error-coqc
;;
;; This error is signaled with one data item -- the file name

(put 'coq-compile-error-coqc 'error-conditions
     '(error coq-compile-error coq-compile-error-coqc))
(put 'coq-compile-error-coqc 'error-message
     "Coq compilation error: coqc fails in")

;; coq-compile-error-command-start
;;
;; This error is signaled with two data items --
;; a list consisting of the command and the system error message,
;; the command itself is a string list of the command name and the options

(put 'coq-compile-error-command-start 'error-conditions
     '(error coq-compile-error coq-compile-error-command-start))
(put 'coq-compile-error-command-start 'error-message
     "Coq compilation error:")

;; coq-compile-error-circular-dep
;;
;; This error is signaled with one data item -- an indication about
;; the circularity, which is the error message to be appended to the
;; contents of 'error-message.

(put 'coq-compile-error-circular-dep 'error-conditions
     '(error coq-compile-error coq-compile-error-circular-dep))
(put 'coq-compile-error-circular-dep 'error-message
     "Coq compilation error: Circular dependency")

;; coq-compile-error-rm
;;
;; Signaled when we have to delete a .vio or .vo file for consistency and
;; that deletion fails.
;;
;; This error is signaled with one data item -- the file-error error
;; description. Its car is the error symbol `file-error' and the cdr are
;; the data items for this error. They seem to be a list of strings with
;; different parts of the error message.

(put 'coq-compile-error-rm 'error-conditions
     '(error coq-compile-error coq-compile-error-rm))
(put 'coq-compile-error-rm 'error-message
     "Cannot remove outdated file.")


;;; find circular dependencies in non-ready compilation jobs

(defun coq-par-find-dependency-circle-for-job (job path)
  "Find a dependency cycle in the dependency subtree of JOB.
Do a depth-first-search to find the cycle. JOB is the current
node and PATH the stack of visited nodes. Jobs in state
'enqueue-coqc can be ignored, because they can never participate
in a cycle."
  ;; CORRECTNESS ARGUMENT FOR THIS FUNCTION
  ;;
  ;; An outside-cycle job is a job for which the whole upward segment
  ;; of the dependency graph does not contain any cycle (and therefore
  ;; neither job itself belongs to any cycle nor is there a path
  ;; starting in job and leading to a cycle). Any job is either
  ;; outside-cycle or there exists an upward path containing the job
  ;; and a cycle above it.
  ;; The visited-condition is that all jobs marked visited are
  ;; outside-cycle jobs.
  ;; 
  ;; The precondition of this function is that
  ;; - PATH does not contain any job twice,
  ;; - PATH is an reversed upward path where JOB is a dependant of the
  ;;   head of PATH, and
  ;; - the visited-condition holds.
  ;;
  ;; This function returns nil and the visited condition holds at
  ;; return, if the precondition holds and JOB is outside-cycle.
  ;; Proved by induction on the length of the maximal upward path.
  ;; 
  ;; This function returns a cycle (and not nil) if the precondition
  ;; holds and JOB is not outside-cycle. Proved by induction on the
  ;; distance to the first repeated job on the right-most upward path
  ;; containing a cycle. Here, right is the direction of the head of
  ;; the 'coqc-dependants list.
  (let (cycle (p path))
    ;; path is reversed. A potential cycle goes from the beginning of
    ;; path to the first occurence of job.
    (while p
      (when (eq (car p) job)
	(setcdr p nil)
	(setq cycle path))
      (setq p (cdr p)))
    (if cycle
	cycle
      (setq path (cons job path))
      (setq p (get job 'coqc-dependants))
      (while (and p (not cycle))
        ;; XXX when clause does not make sense: job is a dependee or
        ;; dependency of (car p), therefore (car p) can only be in
        ;; state 'waiting-dep
        ;; XXX only recurse for dependants not visited yet
	(when (eq (get (car p) 'state) 'waiting-dep)
          ;; XXX should not be called on visited ancestors?
	  (setq cycle (coq-par-find-dependency-circle-for-job (car p) path)))
	(setq p (cdr p)))
      (put job 'visited t)
      cycle)))

(defun coq-par-find-dependency-circle ()
  "Find a dependency cycle in compilation jobs of state 'waiting-dep.
If no circle is found return nil, otherwise the list of files
belonging to the circle.  Jobs in state 'enqueue-coqc can be
ignored, because they can never participate in a cycle."
  (let (cycle)
    (maphash (lambda (_key job) (put job 'visited nil))
	     coq--compilation-object-hash)
    (maphash
     (lambda (_key job)
       (when (and (not cycle) (not (get job 'visited))
		  (eq (get job 'state) 'waiting-dep))
	 (setq cycle (coq-par-find-dependency-circle-for-job job nil))))
     coq--compilation-object-hash)
    (mapcar (lambda (j) (get j 'src-file)) cycle)))


;;; map coq module names to files, using coqdep

(defun coq-par-coqdep-warning-arguments ()
  "Return a fresh list with the warning command line arguments for coqdep.
Warnings (`-w`) are supported in coqdep from 8.19 onwards.
Therefore return the empty list for a detected Coq version
earlier than 8.19."
  (if (and (coq--post-v818) (consp coq-compile-coqdep-warnings))
      (list "-w" (mapconcat #'identity coq-compile-coqdep-warnings ","))
    ()))

(defun coq-par-coqdep-arguments (lib-src-file clpath)
  "Compute the command line arguments for invoking coqdep on LIB-SRC-FILE.
Argument CLPATH must be `coq-load-path' from the buffer
that triggered the compilation, in order to provide correct
load-path options to coqdep."
  (nconc (copy-sequence coq-compile-extra-coqdep-arguments) ; copy for nconc
         (coq-par-coqdep-warning-arguments)
         (coq-coqdep-prog-args clpath
                               (file-name-directory lib-src-file)
                               (coq--pre-v85))
         (list lib-src-file)))

(defun coq-par-coqc-arguments (lib-src-file clpath)
  "Compute the command line arguments for invoking coqc on LIB-SRC-FILE.
Argument CLPATH must be `coq-load-path' from the buffer
that triggered the compilation, in order to provide correct
load-path options to coqdep."
  (nconc (copy-sequence coq-compile-extra-coqc-arguments) ; copy for nconc
         (coq-coqc-prog-args clpath
                             (file-name-directory lib-src-file)
                             (coq--pre-v85))
         (list lib-src-file)))

(defun coq-par-analyse-coq-dep-exit (status output command)
  "Analyse output OUTPUT of coqdep command COMMAND with exit status STATUS.
Returns the list of .vo dependencies if there is no error. Otherwise,
writes an error message into `coq--compile-response-buffer', makes
this buffer visible and returns a string.

This function does always return .vo dependencies, regardless of the
value of `coq-compile-quick'. If necessary, the conversion into .vio
or .vos files must be done elsewhere."
  ;; (when coq--debug-auto-compilation
  ;;   (message "analyse coqdep output \"%s\"" output))
  (if (or
       (not (eq status 0))
       ;; when a file on the command line is missing, coqdep drops it,
       ;; possibly outputting nothing
       (equal output "")
       ;; when a dependency is missing coqdep outputs a warning with status 0
       (string-match coq-coqdep-error-regexp output))
      (progn
	;; display the error
	(coq-compile-display-error
         (mapconcat #'identity command " ")
         (if (equal output "")
             "No coqdep output - file probably inaccessible"
           output)
         t)
        ;; give back a string to signal error - the string content
        ;; will only become visible during debugging
        (cond
         ((not (eq status 0))
          (format "coqdep exit status %d" status))
         ((equal output "")
          "no coqdep output")
         (t
          "unsatisfied dependencies")))
    ;; In 8.5, coqdep produces two lines. Match with .* here to
    ;; extract only a part of the first line.
    ;; We could match against (concat "^[^:]*" obj-file "[^:]*: \\(.*\\)")
    ;; to select the right line for either .vo or .vio dependencies.
    ;; However, we want to accept a .vo prerequisite for a .vio target
    ;; if it is recent enough. Therefore we actually need module dependencies
    ;; instead of file dependencies and we derive them from the .vo line.
    (when (string-match "\\`[^:]*: \\(.*\\)" output)
      (cl-remove-if-not
       (lambda (f) (string-match-p "\\.vo\\'" f))
       (split-string (match-string 1 output))))))


;;; manage background jobs

(defun coq-par-kill-all-processes ()
  "Kill all background coqc, coqdep, vio2vo or vok compilation processes.
Return t if some process was killed."
  ;; need to first mark processes as killed, because delete process
  ;; starts running sentinels in the order processes terminated, so
  ;; after the first delete-process we see sentinels of non-killed
  ;; processes running
  (let ((kill-needed))
    (mapc
     (lambda (process)
       (when (process-get process 'coq-compilation-job)
	 (process-put process 'coq-par-process-killed t)
	 (setq kill-needed t)))
     (process-list))
    (when kill-needed
      (message "Kill all Coq background compilation processes"))
    (mapc
     (lambda (process)
       (when (process-get process 'coq-compilation-job)
	 (process-put process 'coq-par-process-killed t)
	 (delete-process process)
	 (when coq--debug-auto-compilation
	   (message "%s %s: kill it"
		    (get (process-get process 'coq-compilation-job) 'name)
		    (process-name process)))))
     (process-list))
    (setq coq--current-background-jobs 0)
    kill-needed))

(defun coq-par-unlock-all-ancestors-on-error ()
  "Unlock ancestors which are not in an asserted span.
Used for unlocking ancestors on compilation errors."
  (when coq--compilation-object-hash
    (maphash
     (lambda (_key job)
       (when (eq (get job 'lock-state) 'locked)
         (coq-unlock-ancestor (get job 'src-file))
	 (put job 'lock-state 'unlocked)))
     coq--compilation-object-hash)))

(defun coq-par-kill-and-cleanup ()
  "Kill all background compilation, cleanup internal state and unlock ancestors.
This is the common core of `coq-par-emergency-cleanup' and
`coq-par-user-interrupt'.  Returns t if there actually was a
background job that was killed."
  (let (proc-killed)
    (when coq--debug-auto-compilation
      (message "kill all jobs and cleanup state"))
    (setq proc-killed (coq-par-kill-all-processes))
    (setq coq--par-compilation-queue (coq-par-new-queue))
    (setq coq--last-compilation-job nil)
    (setq coq--par-second-stage-queue (coq-par-new-queue))
    (setq coq--par-second-stage-in-progress nil)
    (when coq--par-second-stage-delay-timer
      (cancel-timer coq--par-second-stage-delay-timer)
      (setq coq--par-second-stage-delay-timer nil))
    (coq-par-unlock-all-ancestors-on-error)
    (setq proof-second-action-list-active nil)
    (coq-par-init-compilation-hash)
    proc-killed))

(defun coq-par-emergency-cleanup ()
  "Emergency cleanup for errors in parallel background compilation.
This is the function that cleans everything up when some
background compilation process detected a severe error.  When
`coq-compile-keep-going' is nil, all errors are severe.  When
`coq-compile-keep-going' is t, coqc and coqdep errors are
not severe.  This function is also used for the user action to
kill all background compilation.

On top of `coq-par-kill-and-cleanup', this function resets the
queue region (and thus `proof-action-list' and signals an
interrupt to the proof shell."
  (interactive)				; needed for menu
  (when coq--debug-auto-compilation
    (message "emergency cleanup"))
  (coq-par-kill-and-cleanup)
  (when proof-action-list
    (setq proof-shell-interrupt-pending t))
  (proof-release-lock)
  (proof-detach-queue))

(defun coq-par-user-interrupt ()
  "React to a generic user interrupt with cleaning up everything.
This function cleans up background compilation when the proof
assistant died (`proof-shell-handle-error-or-interrupt-hook') or
when the user interrupted Proof General (with \\[proof-interrupt-process] or
`proof-interrupt-process' leading to
`proof-shell-signal-interrupt-hook').

On top of `coq-par-kill-and-cleanup', this function only sets the
dynamic variable `prover-was-busy' to tell the proof shell that
the user actually had a reason to interrupt.  However, in the
special case where `proof-action-list' is nil and
`coq--last-compilation-job' not, this function also clears the
queue region and releases the proof shell lock.  This has the nice
side effect, that `proof-interrupt-process' does not send an
interrupt signal to the prover."
  (let (proc-killed
	(was-busy (or coq--last-compilation-job
		      coq--par-second-stage-in-progress
		      coq--par-second-stage-delay-timer)))
    (when coq--debug-auto-compilation
      (message "cleanup on user interrupt"))
    (setq proc-killed (coq-par-kill-and-cleanup))
    (unless proof-action-list
      (when coq--debug-auto-compilation
	(message "clear queue region and proof shell lock"))
      (proof-release-lock)
      (proof-detach-queue))
    (when (and (boundp 'prover-was-busy)
	       (or proc-killed was-busy))
      (setq prover-was-busy t))))

(defun coq-par-process-filter (process output)
  "Store OUTPUT from coq background compilation."
  (process-put process 'coq-process-output
	       (concat (process-get process 'coq-process-output) output)))

(defun coq-par-start-process (command arguments continuation job file-rm)
  "Start asynchronous compilation job for COMMAND.
This function starts COMMAND with arguments ARGUMENTS for
compilation job JOB, making sure that CONTINUATION runs when the
process finishes successfully. FILE-RM, if non-nil, denotes a
file to be deleted when the process does not finish successfully."
  ;;(message "CPSP %s %s %s %s %s" command arguments continuation job file-rm)
  (let ((process-connection-type nil)	; use pipes
	(process-name (format "pro-%s" coq--par-next-id))
	process)
    (setq coq--par-next-id (1+ coq--par-next-id))
    (when coq--debug-auto-compilation
      (message "%s %s: start %s %s in %s"
	       (get job 'name) process-name
	       command
               (mapconcat (lambda (a) (concat "\"" a "\"")) arguments " ")
	       default-directory))
    (condition-case err
	;; If the command is wrong, start-process aborts with an
	;; error. However, in Emacs 23.4.1. it will leave a process
	;; behind, which is in a very strange state: running with no
	;; pid. Emacs 24.2 fixes this.
	(setq process (apply #'start-process process-name
			     nil	; no process buffer
			     command arguments))
      (error
       (when coq--debug-auto-compilation
         (message "%s %s: error in start process, %s"
		  (get job 'name) process-name
                  (if file-rm
                      (format "rm %s" file-rm)
                    "no file removal")))         
       (when file-rm
	 (ignore-errors (delete-file file-rm)))
       (signal 'coq-compile-error-command-start
	       (list (cons command arguments) (nth 2 err)))))
    (set-process-filter process #'coq-par-process-filter)
    (set-process-sentinel process #'coq-par-process-sentinel)
    (set-process-query-on-exit-flag process nil)
    (setq coq--current-background-jobs (1+ coq--current-background-jobs))
    (process-put process 'coq-compilation-job job)
    (process-put process 'coq-process-continuation continuation)
    (process-put process 'coq-process-command (cons command arguments))
    (process-put process 'coq-process-output "")
    (process-put process 'coq-process-rm file-rm)))

(defun coq-par-process-sentinel (process event)
  "Sentinel for all kinds of Coq background compilation processes.
Runs when process PROCESS terminated because of EVENT. It
determines the exit status and calls the continuation function
that has been registered with that process. Normal compilation
errors are reported with an error message inside the callback.
Starts as many queued jobs as possible. The callback and queued
jobs are done with the 'script-buf as current buffer, such that
local variables and `default-directory' have correct values.
Second stage compilation jobs that have been killed, possibly
because the user triggered a next first stage compilation, are
put back into `coq--par-second-stage-queue'. If, at the end, no
job is running in the background but compilation has not been
finished, then, either second stage compilation finished (which
is reported), or there must be a cycle in the dependencies, which
is found and reported here. The cycle detection is skipped, if
the retirement of the last compilation job has been delayed per
`coq--par-delayed-last-job'. All signals are caught inside this
function and reported appropriately."
  (condition-case err
      (if (process-get process 'coq-par-process-killed)
	  (progn
	    (when coq--debug-auto-compilation
	      (message "%s %s: skip sentinel, process killed, %s"
		       (get (process-get process 'coq-compilation-job) 'name)
		       (process-name process)
		       (if (process-get process 'coq-process-rm)
			   (format "rm %s"
				   (process-get process 'coq-process-rm))
			 "no file removal")))
	    (when (process-get process 'coq-process-rm)
	      (ignore-errors
		(delete-file (process-get process 'coq-process-rm))))
	    (when (or
                   (eq (process-get process 'coq-process-continuation)
		       'coq-par-vok-continuation)
                   (eq (process-get process 'coq-process-continuation)
		       'coq-par-vio2vo-continuation))
	      (when coq--debug-auto-compilation
		(message "%s: reenqueue for second stage"
			 (get (process-get process 'coq-compilation-job) 'name)))
	      (coq-par-second-stage-enqueue
	       (process-get process 'coq-compilation-job))))
        ;; process was not killed explicitly by us
        (with-current-buffer
            (get (process-get process 'coq-compilation-job) 'script-buf)
	  (let (exit-status)
	    (when coq--debug-auto-compilation
	      (message
               (concat "%s %s: TTT process status changed to %s  "
                       "command: %s\n  default-dir: %s curr buf %s")
	       (get (process-get process 'coq-compilation-job) 'name)
	       (process-name process)
	       event
               (mapconcat 'identity
                          (process-get process 'coq-process-command)
                          " ")
               default-directory
               (buffer-name)))
	    (cond
	     ((eq (process-status process) 'exit)
	      (setq exit-status (process-exit-status process)))
	     (t (setq exit-status "abnormal termination")))
	    (setq coq--current-background-jobs
		  (max 0 (1- coq--current-background-jobs)))
	    (funcall (process-get process 'coq-process-continuation)
		     process exit-status)
	    (coq-par-start-jobs-until-full)
	    (when (and coq--par-second-stage-in-progress
		       (eq coq--current-background-jobs 0))
	      (setq coq--par-second-stage-in-progress nil)
              (if (coq--post-v811)
                  (message "vok compilation finished")
	        (message "vio2vo compilation finished")))
	    (when (and
		   (not coq--par-delayed-last-job)
		   (eq coq--current-background-jobs 0)
		   coq--last-compilation-job)
	      (let ((cycle (coq-par-find-dependency-circle)))
	        (if cycle
		    (signal 'coq-compile-error-circular-dep
			    (mapconcat #'identity cycle " -> "))
		  (error "Deadlock in parallel compilation")))))))
    ;; coq-compile-error-start can be signaled inside the continuation
    ;; function, if that tries to start new jobs
    ;; XXX catch this error also in coq-par-preprocess-require-commands
    (coq-compile-error-command-start
     (coq-par-emergency-cleanup)
     (message "%s \"%s\" in \"%s\""
	      (get (car err) 'error-message)
	      (nth 2 err) (mapconcat 'identity (cadr err) " ")))
    (coq-compile-error
     (coq-par-emergency-cleanup)
     (message "%s %s" (get (car err) 'error-message) (cdr err)))
    (error
     (message "Error in sentinel of process %s, error %s"
	      (process-name process) err)
     (coq-par-emergency-cleanup)
     (signal (car err) (cdr err)))))


;;; second stage compilation (vio2vo and vok)

(defun coq-par-run-second-stage-queue ()
  "Start delayed second stage compilation (vio2vo or vok).
Use the buffer stored in the 'script-buf property as current
buffer for starting processes, such that local variables and, in
particular, `default-directory' have the correct values."
  ;; when the user starts another compilation, the timer for second
  ;; stage is canceled
  (cl-assert (not coq--last-compilation-job)
	  nil "normal compilation and second stage in parallel 1")
  (setq coq--par-second-stage-delay-timer nil)
  (when coq--debug-auto-compilation
    (message "Start second stage processing for %d jobs"
             (coq-par-second-stage-queue-length)))
  (let ((head (coq-par-second-stage-head)))
    (when head
      (with-current-buffer (get head 'script-buf)
        (setq coq--par-second-stage-in-progress t)
        (coq-par-start-jobs-until-full)))))

(defun coq-par-require-processed (race-counter)
  "Callback for `proof-action-list' to signal completion of the last Require.
This function ensures that second stage compilation starts after
`coq-compile-second-stage-delay' seconds after the last module has been
loaded into Coq. When background compilation is successful, this
callback is inserted with a dummy item into proof-action-list
somewhere after the last require command. RACE-COUNTER is used to
detect more than one active callback, see
`coq--par-second-stage-start-id'"
  ;; When the user asserts new stuff while the (previously) last
  ;; require command is being processed, `coq--last-compilation-job'
  ;; might get non-nil. In this case there is a new last compilation
  ;; job that will eventually trigger the second stage.
  (unless (or coq--last-compilation-job
	      (not (eq race-counter coq--par-second-stage-start-id)))
    (setq coq--par-second-stage-delay-timer
	  (run-at-time coq-compile-second-stage-delay nil
		       #'coq-par-run-second-stage-queue))))

(defun coq-par-callback-queue-item (callback)
  "Create queue item containing just CALLBACK.
Create a queue item for `proof-action-list' containing just
CALLBACK. CALLBACK must be a function taking a span as argument.
The command list in the produced queue item is nil, therefore the
item will be processed as comment and only the callback will be called."
  ;; A proof-action-list item has the form of
  ;;            (SPAN COMMANDS ACTION [DISPLAYFLAGS])
  ;; If COMMANDS is nil, the item is processed as comment and not sent
  ;; to the proof assistant, only the callback is called, see
  ;; proof-shell.el.
  (list nil nil callback))


;;; background job tasks

(defun coq-par-dependencies-ready (job)
  "Return t if all dependencies of compilation job JOB are ready."
  (eq (get job 'coqc-dependency-count) 0))

(defun coq-par-add-coqc-dependency (dependee dependant count)
  "Add normal Coq dependency from child job DEPENDEE to parent job DEPENDANT.
If argument COUNT is non-nil, the dependency counter in DEPENDANT
is increased, such that DEPENDANT waits for the completion of DEPENDEE."
  (when count
    (put dependant 'coqc-dependency-count
         (1+ (get dependant 'coqc-dependency-count))))
  (push dependant (get dependee 'coqc-dependants))
  (push dependee (get dependant 'coqc-dependees))
  (when coq--debug-auto-compilation
    (message "%s -> %s: add %s coqc dependency"
	     (get dependee 'name) (get dependant 'name)
             (if count "counted" "uncounted"))))

;; XXX need to propagate 'failed from dependee to dependant
;; Relevant, if user asserts second region when for the first one
;; compilation has failed already, but the queue region is still
;; processing. What happens with coq--par-delayed-last-job and the
;; delayed coq-par-kickoff-queue-maybe call?
(defun coq-par-add-queue-dependency (dependee dependant)
  "Add queue dependency from require job DEPENDEE to require job DEPENDANT.
The require item of DEPENDANT comes after those of DEPENDEE.
Therefore DEPENDANT must wait for DEPENDEE to finish. "
  (cl-assert (and (not (get dependant 'queue-dependant-waiting))
                  (not (get dependee 'queue-dependant)))
	  nil "queue dependency cannot be added")
  (put dependant 'queue-dependant-waiting t)
  (put dependee 'queue-dependant dependant)
  (put dependant 'queue-dependee dependee)
  (when coq--debug-auto-compilation
    (message "%s -> %s: add queue dependency"
	     (get dependee 'name) (get dependant 'name))))

(defun coq-par-job-needs-compilation-quick (job)
  "Determine whether JOB needs to get compiled and do some side effects.
This function contains most of the logic nesseary to support
quick compilation according to `coq-compile-quick' for coq < 8.11.  Taking
`coq-compile-quick' into account, it determines if a compilation
is necessary.  The property 'required-obj-file is set either to
the file that we need to produce or to the up-to-date object
file.  If compilation is needed, property 'use-quick is set to `vio' when
-quick/-vio will be used.  If no compilation is needed, property
'obj-mod-time remembers the time stamp of 'required-obj-file.
Indepent of whether compilation is required, .vo or .vio files
that are in the way are deleted.  Note that the coq documentation
does not contain a statement, about what file is loaded, if both
a .vo and a .vio file are present.  To be on the safe side, I
therefore delete a file if it might be in the way.  Sets the
'second-stage property on job if necessary."
  (let* ((vo-file (get job 'vo-file))
	 (vio-file (coq-library-vio-of-vo-file vo-file))
	 (vo-obj-time (nth 5 (file-attributes vo-file)))
	 (vio-obj-time (nth 5 (file-attributes vio-file)))
	 (dep-time (get job 'youngest-coqc-dependency))
	 (src-time (nth 5 (file-attributes (get job 'src-file))))
	 file-to-delete max-obj-time vio-is-newer
	 other-file other-obj-time result)
    (when coq--debug-auto-compilation
      (message
       (concat "%s: compare mod times: vo mod %s, vio mod %s, src mod %s, "
	       "youngest dep %s; vo < src : %s, vio < src : %s, "
	       "vo < dep : %s, vio < dep : %s")
       (get job 'name)
       (if vo-obj-time (current-time-string vo-obj-time) "-")
       (if vio-obj-time (current-time-string vio-obj-time) "-")
       (if src-time (current-time-string src-time) "-")
       (if (eq dep-time 'just-compiled) "just compiled"
	 (current-time-string dep-time))
       (if vo-obj-time (time-less-p vo-obj-time src-time) "-")
       (if vio-obj-time (time-less-p vio-obj-time src-time) "-")
       (if vo-obj-time (coq-par-time-less vo-obj-time dep-time) "-")
       (if vio-obj-time (coq-par-time-less vio-obj-time dep-time) "-")))
    ;; the source file must exist
    (cl-assert src-time nil
               "internal error - cannot find src file I")
    ;; Compute first the max of vo-obj-time and vio-obj-time and remember
    ;; which of both is newer. This is only meaningful if at least one of
    ;; the .vo or .vio file exists.
    (cond
     ((and vio-obj-time vo-obj-time
	   (time-less-or-equal vo-obj-time vio-obj-time))
      (setq max-obj-time vio-obj-time)
      (setq vio-is-newer t))
     ((and vio-obj-time vo-obj-time)
      (setq max-obj-time vo-obj-time))
     (vio-obj-time
      (setq max-obj-time vio-obj-time)
      (setq vio-is-newer t))
     (t
      (setq max-obj-time vo-obj-time)))
    ;; Decide if and what to compile.
    (if (or (eq dep-time 'just-compiled) ; a dep has been just compiled
	    (and (not vo-obj-time) (not vio-obj-time)) ; no obj exists
	    ;; src younger than any obj?
	    (time-less-or-equal max-obj-time src-time)
	    ;; dep younger than any obj?
	    (time-less-or-equal max-obj-time dep-time))
	;; compilation is definitely needed
	(progn
	  (setq result t)
	  (if (coq-compile-prefer-quick)
	      (progn
		(put job 'required-obj-file vio-file)
		(put job 'use-quick 'vio)
		(when vo-obj-time
		  (setq file-to-delete vo-file))
		(when (eq coq-compile-quick 'quick-and-vio2vo)
		  (put job 'second-stage 'vio2vo)))
	    (put job 'required-obj-file vo-file)
	    (when vio-obj-time
	      (setq file-to-delete vio-file)))
	  (when coq--debug-auto-compilation
	    (message
	     (concat "%s: definitely need to compile to %s; delete %s")
	     (get job 'name)
	     (get job 'required-obj-file)
	     (if file-to-delete file-to-delete "noting"))))
      ;; Either the .vio or the .vo file exists and one of .vio or .vo is
      ;; younger than the source and the youngest dependency. Might not
      ;; need to compile.
      (if (eq coq-compile-quick 'ensure-vo)
	  (progn
	    (put job 'required-obj-file vo-file)
	    (if (or (not vio-is-newer) ; vo is newest
		    (and vo-obj-time   ; vo is older than vio
			               ; but still newer than src or dep
			 (time-less-p src-time vo-obj-time)
			 (time-less-p dep-time vo-obj-time)))
		;; .vo is newer than src and youngest dep - don't compile
		(progn
		  (put job 'obj-mod-time vo-obj-time)
		  ;; delete vio if it is outdated or newer than vo
		  (when (and vio-obj-time
			     (or vio-is-newer
				 (time-less-or-equal vio-obj-time src-time)
				 (time-less-or-equal vio-obj-time dep-time)))
		    (setq file-to-delete vio-file))
		  (when coq--debug-auto-compilation
		    (message "%s: vo up-to-date 1; delete %s"
			     (get job 'name)
			     (if file-to-delete file-to-delete "noting"))))
	      ;; .vo outdated - need to compile
	      (setq result t)
	      ;; delete vio if it is outdated
	      (when (and vio-obj-time
			 (or (time-less-or-equal vio-obj-time src-time)
			     (time-less-or-equal vio-obj-time dep-time)))
		(setq file-to-delete vio-file))
	      (when coq--debug-auto-compilation
		(message "%s: need to compile to vo; delete %s"
			 (get job 'name)
			 (if file-to-delete file-to-delete "noting")))))
	;; There is an up-to-date .vio or .vo file and the user does not
	;; insist on either .vio or .vo - no need to compile.
	;; Ensure to delete outdated .vio or .vo files.
	;; First store the other obj file in other-file and other-obj-time.
	(if vio-is-newer
	    (setq other-file vo-file
		  other-obj-time vo-obj-time)
	  (setq other-file vio-file
		other-obj-time vio-obj-time))
	(if (and other-obj-time
		 (time-less-p src-time other-obj-time)
		 ;; dep-time is neither nil nor 'just-compiled here
		 (time-less-p dep-time other-obj-time))
	    ;; Both the .vio and .vo exist and are up-to-date. Coq
	    ;; loads the younger one but we continue with the older
	    ;; one to avoid recompilation for the case where a vio2vo
	    ;; process took a long time for a dependency.
	    (progn
	      (put job 'required-obj-file other-file)
	      (put job 'obj-mod-time other-obj-time)
	      (when coq--debug-auto-compilation
		(message (concat "%s: .vio and .vo up-to-date, "
				 "continue with the older %s")
			 (get job 'name)
			 (if vio-is-newer ".vio" ".vo"))))
	  ;; The other obj file does not exist or is outdated.
	  ;; Delete the outdated if it exists.
	  (when other-obj-time
	    (setq file-to-delete other-file))
	  (if vio-is-newer
	      (progn
		(put job 'required-obj-file vio-file)
		(put job 'obj-mod-time vio-obj-time)
		(when (eq coq-compile-quick 'quick-and-vio2vo)
		  (put job 'second-stage 'vio2vo))
		(when coq--debug-auto-compilation
		  (message "%s: vio up-to-date; delete %s"
			   (get job 'name)
			   (if file-to-delete file-to-delete "noting"))))
	    (put job 'required-obj-file vo-file)
	    (put job 'obj-mod-time vo-obj-time)
	    (when coq--debug-auto-compilation
	      (message "%s: vo up-to-date 2; delete %s"
		       (get job 'name)
		       (if file-to-delete file-to-delete "noting")))))))
    (when file-to-delete
      (condition-case err
	  (delete-file file-to-delete)
	(file-error
	 (signal 'coq-compile-error-rm err))))
    result))

(defun coq-par-job-needs-compilation-vos (job)
  "Determine whether JOB needs to get compiled.
This function decides whether JOB needs to get compiled for coq
>= 8.11 and whether a .vo or a .vos should be produced. For the
latter, `coq-compile-vos' is consulted and, if that is `nil',
coq-compile-quick, see `coq-compile-prefer-vos'. This function
assumes that coq is used consistently and that a .vo file cannot
be present without a .vos file that has the same time stamp or
has been created more recently. As result, this function sets the
property 'use-quick to `vos' if JOB should be compiled with -vos.
If compilation is needed, 'required-obj-file is set.
If no compilation is needed, 'obj-mod-time is set to the time stamp of
the .vos or .vo file, depending on `coq-compile-prefer-vos'.  Sets
the 'second-stage property on job to 'vok if necessary."
  (let* ((vo-file (get job 'vo-file))
         (vos-file (coq-library-vos-of-vo-file vo-file))
         (dep-time (get job 'youngest-coqc-dependency))
	 (src-time (nth 5 (file-attributes (get job 'src-file))))
         (vos-obj-time (nth 5 (file-attributes vos-file)))
         result)
    (when coq--debug-auto-compilation
      (message
       (concat "%s: compare mod times: vos mod %s, src mod %s, youngest dep %s\n"
               "\tsrc file %s in %s")
               (get job 'name)
               (if vos-obj-time (current-time-string vos-obj-time) "-")
               (if src-time (current-time-string src-time) "-")
               (if (eq dep-time 'just-compiled) "just compiled"
	         (current-time-string dep-time))
               (get job 'src-file)
               default-directory))
    ;; the source file must exist
    (cl-assert src-time nil
               "internal error - cannot find src file II")
    (if (or (eq dep-time 'just-compiled) ; a dep has been just compiled
            (not vos-obj-time)           ; neither vos nor vo present
	    ;; src younger than vos?
	    (time-less-or-equal vos-obj-time src-time)
	    ;; dep younger than vos?
	    (time-less-or-equal vos-obj-time dep-time))
	;; compilation needed
        (if (coq-compile-prefer-vos)
            (progn
    	      (put job 'required-obj-file vos-file)
	      (put job 'use-quick 'vos)
              (when (eq coq-compile-vos 'vos-and-vok)
                (put job 'second-stage 'vok))
              (setq result t))
          (put job 'required-obj-file vo-file)
          (setq result t))
      ;; maybe need to compile if vo is required
      (if (coq-compile-prefer-vos)
          ;; vo not required and vos is fine - no compilation, but maybe vok
          (let* ((vok-file (coq-library-vok-of-vo-file vo-file))
                 (vok-time (nth 5 (file-attributes vok-file))))
            (put job 'obj-mod-time vos-obj-time)
            (when (and (eq coq-compile-vos 'vos-and-vok)
                       (or (not vok-time) ; no vok exists
                           ;; vos/vo is younger than src and dep, see above
                           ;; if vo present, vok and vo have often the same time
                           ;; vos/vo younger than vok?
                           (time-less-p vok-time vos-obj-time)))
              (put job 'second-stage 'vok)))
        ;; vo required, may need to compile
        (let ((vo-obj-time (nth 5 (file-attributes vo-file))))
          (when coq--debug-auto-compilation
            (message "%s: ensure-vo: vo mod %s"
                     (get job 'name)
                     (if vo-obj-time (current-time-string vo-obj-time) "-")))
          (if (or (not vo-obj-time)     ; vo not present
	          ;; src younger than vo?
	          (time-less-or-equal vo-obj-time src-time)
	          ;; dep younger than vo?
	          (time-less-or-equal vo-obj-time dep-time))
	      ;; compilation needed
              (progn
                (put job 'required-obj-file vo-file)
                (setq result t))
            ;; vo is fine - no compilation
            (put job 'obj-mod-time vo-obj-time)))))
    result))

(defun coq-par-job-needs-compilation (job)
  "Determine if JOB nees to get compiled and possibly do some side effects.
This function calls `coq-par-job-needs-compilation-vos for coq >=
8.11 and `coq-par-job-needs-compilation-quick' otherwise. Returns
t if a compilation is required and sets the 'use-quick property
depending on wheter -quick/-vio or -vos should be used.
If compilation is needed, 'required-obj-file is set.  Property
'obj-mod-time is set when no compilation is needed."
  (if (coq--post-v811)
      (coq-par-job-needs-compilation-vos job)
    (coq-par-job-needs-compilation-quick job)))

(defun coq-par-collect-locked-ancestors-dependees (job)
  "Collect locked, not-yet-found ancestors from ancestors of JOB.
Apply `coq-par-collect-locked-file-ancestors' recursively to all
dependees to return those ancestors that are not yet asserted and
have not been returned yet by a previous invocation of this
function on a different job. This function sets the
'collect-visited property on all returned jobs, which should be
cleared before the next collection run."
  ;; (message "CLAD: job %s: dependees %s"
  ;;          (get job 'name)
  ;;          (mapconcat
  ;;           (lambda (job) (get job 'name))
  ;;           (get job 'coqc-dependees) " "))
  (apply #'nconc (mapcar #'coq-par-collect-locked-file-ancestors
                         (get job 'coqc-dependees))))

(defun coq-par-collect-locked-file-ancestors (job)
  "Collect locked, not-yet-found ancestors of JOB.
Return JOB if JOB is not asserted yet and has not been visited
before by this function. Do the same recursively on all ancestors
to return all not-yet-asserted ancestors of JOB. This function
sets the 'collect-visited property on all returned jobs, which
should be cleared before the next collection run."
  ;; (message "CLFA job %s cv %s ls %s"
  ;;          (get job 'name) (get job 'collect-visited) (get job 'lock-state))
  (when (and (not (get job 'collect-visited))
             (eq (get job 'lock-state) 'locked))
    (put job 'collect-visited t)
    (cons job (coq-par-collect-locked-ancestors-dependees job))))

(defun coq-par-collect-locked-require-ancestors-rec (job)
  "Collect locked, not yet found ancestorors for require job JOB.
Return all not yet asserted ancestors for successful jobs JOB as
well as for failed jobs JOB. For successful jobs, the not yet
asserted ancestors of preceeding require jobs have been collected
in a previous collection run and have been asserted back then.
For failed jobs, this is not the case and, moreover, ancestor
unlocking can only be done when the last failing reqire job is
retired. Therefore, for failed jobs, this function recureses into
the preceeding require job, if it exists and is also marked as
failed."
  (let ((this-anc (coq-par-collect-locked-ancestors-dependees job))
        (q-dep (get job 'queue-dependee))
        prev-anc)
    (when (and q-dep (get q-dep 'failed))
      (setq prev-anc (coq-par-collect-locked-require-ancestors-rec q-dep)))
    (nconc this-anc prev-anc)))

(defun coq-par-collect-locked-require-ancestors (job)
  "Top-level ancestor collection function - collects not asserted ancestors.
Return all not yet asserted ancestors for successful jobs JOB as
well as for failed jobs JOB. For failed require jobs JOB,
additionally collect all asserted ancestors of all preceeding
failed require jobs. This is necessary, because for failed jobs,
unlocking only happens when the last require job is retired. The
recursion internally uses property 'collect-visited to mark
already visited jobs in order to avoid an exponential blowup in
graphs that are not trees. This property is reset here after
collection, such that its use stays internal."
  (let ((ancs (coq-par-collect-locked-require-ancestors-rec job)))
    (mapc (lambda (job) (put job 'collect-visited nil)) ancs)
    (when coq--debug-auto-compilation
      (message "%s: locked ancestors: %s"
               (get job 'name)
               (mapconcat
                (lambda (job) (get job 'name))
                ancs " ")))
    ancs))

(defun coq-par-retire-top-level-job (job)
  "Register ancestors and start queue items.
JOB must be a successful require job.

This function performs the essential tasks for successful require
jobs when they transition from 'waiting-queue to 'ready:
- Registering ancestors in the span and recording this fact in
  the 'lock-state property.
- Moving queue items back to `proof-action-list' and start their
  execution.
- Insert `coq-par-require-processed' as callback if this is the
  last top-level job, such that second stage compilation will start
  eventually."
  (cl-assert (and (not (get job 'failed)) (eq (get job 'type) 'require))
	  nil "coq-par-retire-top-level-job precondition failed")
  (let ((span (get job 'require-span))
	(items (get job 'queueitems)))
    (when coq-lock-ancestors
      (mapc
       (lambda (anc-job)
         (cl-assert (eq (get anc-job 'lock-state) 'locked)
                    nil "bad ancestor lock state")
         (put anc-job 'lock-state 'asserted)
         (push (get anc-job 'src-file)
               (span-property span 'coq-locked-ancestors)))
       (coq-par-collect-locked-require-ancestors job)))
    ;; XXX each require job must have items, right?
    (when items
      (when (and
             (eq coq--last-compilation-job job)
             (or (and (coq--post-v811)
                      (eq coq-compile-vos 'vos-and-vok))
                 (and (not (coq--post-v811))
                      (eq coq-compile-quick 'quick-and-vio2vo))))
	(let ((2nd-stage-counter
	       (setq coq--par-second-stage-start-id
		     (1+ coq--par-second-stage-start-id))))
	  ;; Insert a notification callback for when the last require
	  ;; queue item has been processed.
	  (setq items
		(cons
		 (car items)		; this is the require
		 (cons
		  (coq-par-callback-queue-item
		   (lambda (_span) (coq-par-require-processed
                               2nd-stage-counter)))
		  (cdr items))))))
      (proof-add-to-queue items 'advancing)
      (when coq--debug-auto-compilation
	(message "%s: add %s items to action list"
		 (get job 'name) (length items)))
      ;; this function should only be called once for each require job
      ;; resetting queueitems is on the save side in any case
      (put job 'queueitems nil))))

(defun coq-par-kickoff-queue-from-action-list (job)
  "Trigger `coq-par-kickoff-queue-maybe' from action list.
Simple wrapper around `coq-par-kickoff-queue-maybe' to ensure the
right scripting buffer is the current buffer and local variables
and `default-directory' are taken from there. This function is
used to enter background compilation functions from
`proof-action-list'."
  (with-current-buffer (get job 'script-buf)
    (when coq--debug-auto-compilation
      (message "%s: TTT retry queue kickoff after processing action list"
               (get job 'name)))
    (coq-par-kickoff-queue-maybe job)))

(defun coq-par-kickoff-queue-maybe (job)
  "Transition require job JOB to 'waiting-queue and maybe to 'ready.
This function can only be called for require jobs. It further
must not be called if JOB is in state 'enqueued-coqdep or in
state 'waiting-dep with some not yet finished dependencies. This
function is called when all dependencies of JOB are ready to put
JOB into state 'waiting-dep. When in state 'waiting-dep, this
function is also called, when the queue dependency of JOB has
transitioned to 'ready (inside this function).

First JOB is put into state 'waiting-dep. If there is still a
queue dependency, nothing else happens and JOB waits until the
queue dependee calls this function again when it is ready.

If there is no queue dependency, then require job JOB must be
retired and transition to 'ready. This means:
- for successful require jobs, ancestors are registered in the
  'queue-span and marked as 'asserted in their 'lock-state
  property
- processing of items in 'queueitems is started (if JOB is successful)
- a possible queue dependant gets it's dependency cleared, and,
  if possible the 'waiting-queue -> 'ready transition
  is (recursively) done for the dependant
- if this job is the last top-level compilation
  job (`coq--last-compilation-job') then the last compilation job
  and `proof-second-action-list-active' are cleared and second stage
  processing is triggered.
- If compilation failed, the (failing) last top-level job is
  delayed until `proof-action-list' is empty, possibly by
  registering this call as a callback in an empty
  proof-action-list item. When proof-action-list is empty, the
  queue span is deleted, remaining spans are cleared and the
  `proof-shell-busy' lock is freed."
  (cl-assert (and (eq (get job 'type) 'require)
                  (or (eq (get job 'state) 'waiting-queue)
                      (and (eq (get job 'state) 'waiting-dep)
                           (coq-par-dependencies-ready job))))
             nil "coq-par-kickoff-queue-maybe precondition failed")
  (put job 'state 'waiting-queue)
  (if (get job 'queue-dependant-waiting)
      (when coq--debug-auto-compilation
        (message "%s: no queue kickoff because waiting for queue dependency"
                 (get job 'name)))
    ;; first require job in the queue of require jobs
    (when coq--debug-auto-compilation
      (message "%s: has itself no queue dependency" (get job 'name)))
    (unless (get job 'failed)
      (coq-par-retire-top-level-job job))
    (when (get job 'failed)
      ;; Reset coq--par-delayed-last-job for the case that this
      ;; function was called from a callback in proof-action-list. If
      ;; called from elsewhere, this does not harm.
      (setq coq--par-delayed-last-job nil))
    (if (and (get job 'failed)
	     (eq coq--last-compilation-job job)
	     proof-action-list)
	(progn
	  (when coq--debug-auto-compilation
	    (message "%s: delay queue kickoff until action list is empty"
		     (get job 'name)))
	  (setq coq--par-delayed-last-job t)
	  (proof-add-to-queue
	   (list (coq-par-callback-queue-item
		  (lambda (_span)
                    (coq-par-kickoff-queue-from-action-list job))))
	   'advancing))
      (put job 'state 'ready)
      (when coq--debug-auto-compilation
	(message "%s: ready" (get job 'name)))
      (let ((dependant (get job 'queue-dependant)))
	(if dependant
	    (progn
	      (cl-assert (not (eq coq--last-compilation-job job))
		      nil "coq--last-compilation-job invariant error")
	      (put dependant 'queue-dependant-waiting nil)
              ;; dependant might still wait for dependencies or even
              ;; for coqdep to finish
              (if (eq (get dependant 'state) 'waiting-queue)
                  (progn
                    (when coq--debug-auto-compilation
                      (message
                       "%s -> %s: clear queue dependency, kickoff queue at %s"
                       (get job 'name) (get dependant 'name)
                       (get dependant 'name)))
                    (coq-par-kickoff-queue-maybe dependant)
                    (when coq--debug-auto-compilation
                      (message "%s: queue kickoff finished"
                               (get job 'name))))
                (when coq--debug-auto-compilation
                  (message
                   "%s -> %s: clear queue dependency, no queue kickoff for %s"
                   (get job 'name) (get dependant 'name)
                   (get dependant 'name)))))
          ;; XXX no dependant - this must be the last require job
	  (when (eq coq--last-compilation-job job)
	    (when (get job 'failed)
	      ;; proof-action-list is empty, see above
              ;; unlock ancestors
              (mapc
               (lambda (anc-job)
                 (cl-assert (eq (get anc-job 'lock-state) 'locked)
                            nil "bad ancestor lock state")
                 (when coq--debug-auto-compilation
                   (message "%s: %s unlock because of error"
                            (get anc-job 'name) (get anc-job 'src-file)))
                 (coq-unlock-ancestor (get anc-job 'src-file))
                 (put anc-job 'lock-state 'unlocked))
               (coq-par-collect-locked-require-ancestors job))
	      ;; variables that hold the queue span are buffer local
	      (with-current-buffer (or proof-script-buffer (current-buffer))
		(proof-script-clear-queue-spans-on-error nil))
	      (proof-release-lock)
	      (when (or (and (coq--post-v811)
                             (eq coq-compile-vos 'vos-and-vok))
                        (and (not (coq--post-v811))
                             (eq coq-compile-quick 'quick-and-vio2vo)))
		(cl-assert (not coq--par-second-stage-delay-timer)
			   nil
                           "second stage timer set before last compilation job")
		(setq coq--par-second-stage-delay-timer
		      (run-at-time coq-compile-second-stage-delay nil
				   #'coq-par-run-second-stage-queue))))
	    (setq coq--last-compilation-job nil)
	    (setq proof-second-action-list-active nil)
	    (when coq--debug-auto-compilation
	      (message "clear last compilation job"))
	    (message "Library compilation %s"
		     (if (get job 'failed) "failed" "finished successfully")))
	  (when coq--debug-auto-compilation
	    (message "%s: no queue dependant, queue kickoff finished"
		     (get job 'name))))))))

(defun coq-par-compile-job-maybe (job)
  "Compile JOB after dependencies are ready or start next transitions.
This function can only be called for 'file jobs. It must also be
called for failed jobs to finish all necessary transitions.
First JOB is put into state 'enqueued-coqc.  Then it is determined
if JOB needs compilation, what file must be produced (depending
on `coq-compile-quick') and if a .vio or .vo file must be
deleted.  If necessary, deletion happens immediately.  If JOB needs
compilation, compilation is started or the JOB is enqueued and
JOB stays in 'enqueued-coqc for the time being.  Otherwise, the
transition 'enqueued-coqc -> 'ready is triggered."
  (cl-assert (eq (get job 'type) 'file)
	  nil "wrong job type in coq-par-compile-job-maybe")
  (put job 'state 'enqueued-coqc)
  ;; Note that coq-par-job-needs-compilation sets 'required-obj-file
  ;; if compilation is needed (but it might also get set if no
  ;; compilation is needed). For Coq < 8.11 .vo or .vio files that are
  ;; in the way are deleted. It also sets the 'second-stage property
  ;; if needed.
  (if (and (not (get job 'failed)) (coq-par-job-needs-compilation job))
      (coq-par-start-or-enqueue job)
    (when coq--debug-auto-compilation
      (message "%s: %s, no compilation"
	       (get job 'name)
	       (if (get job 'failed) "failed" "up-to-date")))
    (when (get job 'second-stage)
      (coq-par-second-stage-enqueue job))
    (coq-par-kickoff-coqc-dependants job nil)))

(defun coq-par-decrease-coqc-dependency (dependant dependee-time)
  "Clear Coq dependency and update dependee information in DEPENDANT.
This function handles a Coq dependency from child dependee to
parent dependant when the dependee has finished compilation (ie.
is in state 'ready).  DEPENDANT must be in state
'waiting-dep.  The time of the most recent ancestor is updated, if
necessary using DEPENDEE-TIME.  DEPENDEE-TIME must be an Emacs
time or 'just-compiled.  The dependency
count of DEPENDANT is decreased and, if it reaches 0, the next
transition is triggered for DEPENDANT.  For 'file jobs this is
'waiting-dep -> 'enqueued-coqc and for 'require jobs this is
'waiting-dep -> 'waiting-queue.

This function must be called for failed jobs to complete all
necessary transitions."
  ;(message "%s: CPDCD with time %s" (get dependant 'name) dependee-time)
  (cl-assert (eq (get dependant 'state) 'waiting-dep)
	  nil "wrong state of parent dependant job")
  (when (coq-par-time-less (get dependant 'youngest-coqc-dependency)
			   dependee-time)
    (put dependant 'youngest-coqc-dependency dependee-time))
  (put dependant 'coqc-dependency-count
       (1- (get dependant 'coqc-dependency-count)))
  (cl-assert (<= 0 (get dependant 'coqc-dependency-count))
	  nil "dependency count below zero")
  (when coq--debug-auto-compilation
    (message "%s: coqc dependency count down to %d"
	     (get dependant 'name) (get dependant 'coqc-dependency-count)))
  (when (coq-par-dependencies-ready dependant)
    (if (eq (get dependant 'type) 'file)
        (coq-par-compile-job-maybe dependant)
      (coq-par-kickoff-queue-maybe dependant))))

(defun coq-par-kickoff-coqc-dependants (job just-compiled)
  "Handle transition to state 'ready for file job JOB.
This function can only be called for file jobs and it must also
be called for failed jobs to complete all necessary transitions.
This function is called after compilation has been finished (with
JUST-COMPILED being t) or after determining that compilation was
not necessary or failed (with JUST-COMPILED being nil). This
function sets 'youngest-coqc-dependency to the maximal (youngest)
time stamp of the vo file for this job and all its ancestors.
This function also decreases the dependency counter on all
dependants, propagates 'youngest-coqc-dependency and
starts any necessary state transitions on the
dependants.  Nothing special happens, if this job is successful
but all its dependants are marked failed.  Ancestor unlocking will
be done when the last require job is retired.

For the case that JUST-COMPILED is nil and that JOB has not
failed, this function relies on 'obj-mod-time has been set
before."
  (cl-assert (not (eq (get job 'type) 'require))
             nil "kickoff-coqc-dependants called for require job")
  ;; most actions are not relevant for failed jobs, but do not harm
  (let ((dependant-alive nil)
        ;; take max (youngest) time of this job and all ancestors
        ;;
        ;; If this job has just been compiled then it is clearly
        ;; 'just-compiled. Otherwise it must be 'obj-mod-time, because
        ;; if some ancestor were newer, this just would have been
        ;; compiled. For failed jobs obj-mod-time might be nil, but
        ;; this does not matter.
        (dep-time (if just-compiled 'just-compiled
                    (get job 'obj-mod-time))))
    (put job 'youngest-coqc-dependency dep-time)
    (when coq--debug-auto-compilation
      ;; (let (ancs)
      (message "%s: kickoff %d coqc dependencies with time %s"
	       (get job 'name) (length (get job 'coqc-dependants))
	       (if (eq dep-time 'just-compiled)
		   'just-compiled
		 (current-time-string dep-time))))
    ;; In the recursion coq-par-kickoff-coqc-dependants ->
    ;; coq-par-decrease-coqc-dependency -> coq-par-compile-job-maybe
    ;; -> coq-par-kickoff-coqc-dependants jobs on the path upwards
    ;; must be in state 'ready, otherwise coq-par-ongoing-compilation
    ;; might take one of those as witness for an ongoing compilation.
    (put job 'state 'ready)
    (dolist (dependant (get job 'coqc-dependants))
      (coq-par-decrease-coqc-dependency dependant dep-time)
      (unless (get dependant 'failed)
	(setq dependant-alive t)))
    (when coq--debug-auto-compilation
      (message (concat "%s: coqc kickoff finished, %s dependant alive")
	       (get job 'name)
	       (if dependant-alive "some" "no")))))

(defun coq-par-start-coqdep-on-require (job)
  "Start coqdep for require job JOB.
Write the require statement in JOB into a temporary file and
start coqdep on it. 

This function may be called asynchronously, if the require job
was queued."
  ;; get coq-load-path from job
  ;; check that this really includes the current dir in the arguments
  (let (
        ;; passively supported coq versions start now with >= 8.9.1 --
        ;; do not maintain 8.4 compatibility
        ;; (load-path
        ;;  ;; For coq < 8.5 coqdep needs the current working directory
        ;;  ;; in the load path. This differs from the directory containing
        ;;  ;; 'temp-require-file. Therefore we add it here and tweek
        ;;  ;; coq-load-path-include-current such that coq-coqdep-prog-args
        ;;  ;; does not add the directory containing 'temp-require-file to
        ;;  ;; load-path.
        ;;  (if (and coq-load-path-include-current (coq--pre-v85))
        ;;      (cons (get job 'current-dir) (get job 'load-path))
        ;;    (get job 'load-path)))
        ;; (coq-load-path-include-current nil)
        (require-command
         (mapconcat #'identity (nth 1 (car (get job 'queueitems))) " "))
        (temp-file (make-temp-file "ProofGeneral-coq" nil ".v")))
    (put job 'temp-require-file temp-file)
    (with-temp-file temp-file (insert require-command))
    (when coq--debug-auto-compilation
      (message "%s: TTT start coqdep for require job for file %s"
	       (get job 'name)
	       (get job 'temp-require-file)))
    (coq-par-start-process
     coq-dependency-analyzer
     (coq-par-coqdep-arguments (get job 'temp-require-file) (get job 'load-path))
     'coq-par-process-coqdep-result
     job
     (get job 'temp-require-file))))

(defun coq-par-start-coqdep-on-file (job)
  "Start coqdep for file job JOB.
Lock the source file and start the coqdep background process."
  (when (and coq-lock-ancestors
             ;; the lock state property is initialized from all as
             ;; locked registered files in `proof-included-files-list'
	     (eq (get job 'lock-state) 'unlocked))
    (proof-register-possibly-new-processed-file (get job 'src-file))
    (put job 'lock-state 'locked))
  (when coq--debug-auto-compilation
    (message "%s: TTT start coqdep for file job for file %s"
	     (get job 'name)
	     (get job 'src-file)))
  (coq-par-start-process
   coq-dependency-analyzer
   (coq-par-coqdep-arguments (get job 'src-file) (get job 'load-path))
   'coq-par-process-coqdep-result
   job
   nil))

(defun coq-par-start-coqc (job)
  "Start coqc background compilation for JOB.
Depending on property 'use-quick, vos or quick compilation may be
used."
  (message "Recompile %s%s"
           (cond
            ((eq (get job 'use-quick) 'vos) "-vos ")
            ((eq (get job 'use-quick) 'vio) "-quick ")
            (t ""))
           (get job 'src-file))
  (let ((arguments
         (coq-par-coqc-arguments (get job 'src-file) (get job 'load-path))))
    (cond
     ((eq (get job 'use-quick) 'vos) (push "-vos" arguments))
     ((eq (get job 'use-quick) 'vio) (push "-quick" arguments))
     (t t))
    (when coq--debug-auto-compilation
      (message "%s: TTT start coqc compile for file job for file %s"
	       (get job 'name)
	       (get job 'src-file)))
    (coq-par-start-process
     coq-compiler
     arguments
     'coq-par-coqc-continuation
     job
     (get job 'required-obj-file))))

(defun coq-par-start-vok (job)
  "Start coqc -vok background compilation for JOB."
  (when coq--debug-auto-compilation
    (message "%s start coqc -vok for %s"
             (get job 'name)
             (get job 'src-file)))
  (message "coqc -vok %s" (get job 'src-file))
  (when coq--debug-auto-compilation
    (message "%s: TTT start coqc -vok for file job for file %s"
	     (get job 'name)
	     (get job 'src-file)))
  (let ((arguments
         (coq-par-coqc-arguments (get job 'src-file) (get job 'load-path))))
    (push "-vok" arguments)
    (coq-par-start-process
     coq-compiler
     arguments
     'coq-par-vok-continuation
     job
     nil)))

(defun coq-par-start-vio2vo (job)
  "Start vio2vo background job."
  (let ((arguments (coq-include-options (get job 'load-path)))
	(module (coq-module-of-src-file (get job 'src-file)))
	(default-directory
	  (file-name-directory (file-truename (get job 'src-file)))))
    (when coq--debug-auto-compilation
      (message "%s: start vio2vo for %s"
	       (get job 'name)
	       (get job 'src-file)))
    (message "vio2vo %s" (get job 'src-file))
    (when coq--debug-auto-compilation
      (message "%s: TTT start vio2vo for file job for file %s"
	       (get job 'name)
	       (get job 'src-file)))
    (coq-par-start-process
     ;; in 8.9.1 and before only coqtop accepts -schedule-vio2vo
     ;; after change 103f59e only coqc accepts -schedule-vio2vo
     (if (coq--post-v810) coq-compiler coq-prog-name)
     (nconc arguments (list "-schedule-vio2vo" "1" module))
     'coq-par-vio2vo-continuation
     job
     (get job 'vo-file))))

(defun coq-par-start-task (job)
  "Start the background job for which JOB is waiting.
JOB was at the head of the compilation queue and now either
coqdep, coqc, vio2vo or vok is started for it. This function may be called
synchronously or asynchronously."
  (let ((job-state (get job 'state)))
    (cond
     ((and (eq job-state 'enqueued-coqdep) (eq (get job 'type) 'require))
      (coq-par-start-coqdep-on-require job))
     ((eq job-state 'enqueued-coqdep)
      (coq-par-start-coqdep-on-file job))
     ((eq job-state 'enqueued-coqc)
      (coq-par-start-coqc job))
     ((and (eq job-state 'ready) (eq (get job 'second-stage) 'vok))
      (coq-par-start-vok job))
     ((and (eq job-state 'ready) (eq (get job 'second-stage) 'vio2vo))
      (coq-par-start-vio2vo job))
     (t (cl-assert nil nil "coq-par-start-task with invalid job")))))

(defun coq-par-start-jobs-until-full ()
  "Start background jobs until the limit is reached.
This function may be called synchronously or asynchronously."
  (let ((max-jobs (if coq--par-second-stage-in-progress
		      coq--internal-max-second-stage-jobs
		    coq--internal-max-jobs))
	next-job)
    (while (and (< coq--current-background-jobs max-jobs)
		(setq next-job (if coq--par-second-stage-in-progress
				   (coq-par-second-stage-dequeue)
				 (coq-par-job-dequeue))))
      (coq-par-start-task next-job))))
  
(defun coq-par-start-or-enqueue (new-job)
  "Start NEW-JOB or put it into the queue of waiting jobs.
NEW-JOB goes already into the waiting queue, if the number of
background jobs is one below the limit.  This is in order to leave
room for Proof General. This function might be called
synchronously or asynchronously."
  (if (< (1+ coq--current-background-jobs) coq--internal-max-jobs)
      (coq-par-start-task new-job)
    (coq-par-job-enqueue new-job)))

(defun coq-par-job-init-common (clpath type script-buf)
  "Common initialization for 'require and 'file jobs.
Create a new job of type TYPE and initialize all common fields of
require and file jobs that need an initialization different from
nil. Argument SCRIPT-BUF must be the script buffer that caused
the background compilation."
  (let ((new-job (make-symbol "coq-compile-job-symbol")))
    (put new-job 'name (format "job-%d" coq--par-next-id))
    (setq coq--par-next-id (1+ coq--par-next-id))
    (put new-job 'coqc-dependency-count 0)
    (put new-job 'type type)
    (put new-job 'state 'enqueued-coqdep)
    (put new-job 'script-buf script-buf)
    ;; The ancestor modification time is not really needed in require
    ;; jobs, however, if the field is present, we can treat require
    ;; and file jobs more uniformely.
    (put new-job 'youngest-coqc-dependency '(0 0))
    (put new-job 'load-path clpath)
    new-job))

(defun coq-par-create-require-job (clpath require-items require-span script-buf)
  "Create a new require job for REQUIRE-SPAN.
Create a new require job and initialize its fields. CLPATH is the
load path configured for the current scripting buffer, that is
passed down to all dependencies and used in all compilations.
REQUIRE-ITEMS are the non-require commands following the
REQUIRE-SPAN, they are temporarily stored in the new require job
outside of `proof-action-list'. SCRIPT-BUF must be the script
buffer that caused the background compilation. It is stored in
property 'script-buf and propagated to all dependent jobs. This
buffer is made current in all sentinels and other asynchronously
called functions to ensure local variables and, in particular,
`default-directory' are correct.

This function is called synchronously when asserting. The new
require job is neither started nor enqueued here - the caller
must do this."
  (let* ((coq-load-path clpath)
         (new-job (coq-par-job-init-common clpath 'require script-buf)))
    (put new-job 'require-span require-span)
    (put new-job 'queueitems require-items)
    (when coq--debug-auto-compilation
      (let* ((require-item (car require-items))
             (require-command (mapconcat #'identity (nth 1 require-item) " ")))
        (message "%s: create require job for %s"
                 (get new-job 'name) require-command)))
    new-job))

;; XXX what happens when job exists but has been unlocked because
;; there was some error and it was not used anywhere back then, but
;; job is now needed as a dependency of some other file?
;; XXX what happens if the job exists and is failed?
(defun coq-par-create-file-job (module-vo-file clpath dep-src-file script-buf)
  "Create a new file job for MODULE-VO-FILE.
DEP-SRC-FILE is the source file whose coqdep output we are just
processing and which depends on MODULE-VO-FILE. This argument is
only used in the error message, when MODULE-VO-FILE happens to
coincide with the current scripting buffer (which means we
detected a dependency cycle). SCRIPT-BUF must be the current
scripting buffer and CLPATH must be the load path configured in
there, see also `coq-par-create-require-job'.

If there is a file job for MODULE-VO-FILE, just return this.
Otherwise, create a new file job and initialize its fields. In
particular, initialize its 'lock-state property from the set of
as locked registered files in `proof-included-files-list'.

If a new job is created it is started or enqueued right away."
  (cond
   ((gethash module-vo-file coq--compilation-object-hash))
   (t
    (let* ((coq-load-path clpath)
           (new-job (coq-par-job-init-common clpath 'file script-buf)))
      ;; fields 'required-obj-file and obj-mod-time are implicitely set to nil
      (put new-job 'vo-file module-vo-file)
      (put new-job 'src-file (coq-library-src-of-vo-file module-vo-file))
      (when (equal (get new-job 'src-file)
		   (buffer-file-name proof-script-buffer))
        ;; test this error case - maybe need more files participating
        ;; in the circle?
	(signal 'coq-compile-error-circular-dep
		(concat dep-src-file " -> scripting buffer")))
      (puthash module-vo-file new-job coq--compilation-object-hash)
      (when coq--debug-auto-compilation
	(message "%s: create %s compilation for %s"
		 (get new-job 'name) (get new-job 'type) module-vo-file))
      ;; if the user single-steps through multiple requires, then the
      ;; compilation for the previous require might have been finished
      ;; and cleared, then we might have visited and locked this file
      ;; already
      (if (member (file-truename (get new-job 'src-file))
		  proof-included-files-list)
	  (put new-job 'lock-state 'asserted)
	(put new-job 'lock-state 'unlocked))
      (message "Check %s" (get new-job 'src-file))
      (coq-par-start-or-enqueue new-job)
      new-job))))

(defun coq-par-mark-queue-failing (job)
  "Mark require JOB and its queue dependants with 'failed.
Mark JOB with 'failed and unlock ancestors as appropriate.
Recurse for queue dependants."
  (unless (get job 'failed)
    (put job 'failed t)
    (cl-assert (not (eq (get job 'state) 'ready))
	    nil "coq-par-mark-queue-failing impossible state")
    (when coq--debug-auto-compilation
      (message "%s: mark as failed" (get job 'name)))
    (when (get job 'queue-dependant)
      (coq-par-mark-queue-failing (get job 'queue-dependant)))))

(defun coq-par-mark-job-failing (job)
  "Mark all dependants of JOB as failing and unlock ancestors as appropriate.
Set the 'failed property on all direct and indirect dependants of
JOB.  Along the way, unlock ancestors as determined by
`coq-par-ongoing-compilation'."
  (unless (get job 'failed)
    (put job 'failed t)
    (when coq--debug-auto-compilation
      (message "%s: mark as failed" (get job 'name)))
    (dolist (dependant (get job 'coqc-dependants))
      (coq-par-mark-job-failing dependant))
    (when (get job 'queue-dependant)
      (coq-par-mark-queue-failing (get job 'queue-dependant)))))

(defun coq-par-process-coqdep-result (process exit-status)
  "Coqdep continuation function: Process coqdep output.
This function analyses the coqdep output of PROCESS.  In case of
error, the job is marked as failed or compilation is aborted via
a signal (depending on `coq-compile-keep-going').  If there was no
coqdep error, the following actions are taken.
- temp-require-file for require jobs is deleted
- the job that started PROCESS is put into state 'waiting-dep
- a new job is created for every dependency and registered in the
  dependency tree of all jobs. For dependencies that are 'ready
  already, the most recent ancestor modification time is
  propagated. If a dependency is marked as failed the current job
  is also marked as failed.
- if there are no dependencies (especially if coqdep failed) or
  all dependencies are ready already, the next transition is
  triggered. For file jobs the next transition goes to
  'enqueued-coqc, for require jobs it goes to 'waiting-queue.
- otherwise the current job is left alone until somebody
  decreases its dependency count to 0.

The argument EXIT-STATUS must be the exit status of PROCESS, it
is directly passed to `coq-par-analyse-coq-dep-exit'."
  (let ((job (process-get process 'coq-compilation-job))
	(dependencies-or-error
	 (coq-par-analyse-coq-dep-exit
	  exit-status
	  (process-get process 'coq-process-output)
	  (process-get process 'coq-process-command)))
	job-max-time dependee-failed)
    (if (stringp dependencies-or-error)
        (progn
          (when coq--debug-auto-compilation
            (message "%s coqdep error %s" (get job 'name) dependencies-or-error))
          (if coq-compile-keep-going
              (coq-par-mark-job-failing job)
            (signal 'coq-compile-error-coqdep (get job 'src-file))))

      ;; no coqdep error -- work on dependencies
      (when coq--debug-auto-compilation
	(message "%s: dependencies of %s are %s"
		 (get job 'name)
                 (if (eq (get job 'type) 'file)
                     (get job 'src-file)
                   (get job 'temp-require-file))
                 dependencies-or-error))
      (when (get job 'temp-require-file)
        (ignore-errors (delete-file (get job 'temp-require-file))))      
      (setq job-max-time (get job 'youngest-coqc-dependency))
      (dolist (dep-vo-file dependencies-or-error)
	(unless (coq-compile-ignore-file dep-vo-file)
	  (let* ((dep-job (coq-par-create-file-job dep-vo-file
                                                   (get job 'load-path)
                                                   (get job 'src-file)
                                                   (get job 'script-buf)))
		 (dep-time (get dep-job 'youngest-coqc-dependency)))
            (when (get dep-job 'failed)
              (setq dependee-failed t))
            (when (and (eq (get dep-job 'state) 'ready)
                       (coq-par-time-less job-max-time dep-time))
              ;; the following clause is not needed for require jobs,
              ;; but it doesn't harm either, so keep the code a little
              ;; bit more simple
              (setq job-max-time dep-time))
	    (coq-par-add-coqc-dependency
             dep-job job (not (eq (get dep-job 'state) 'ready))))))
      (put job 'youngest-coqc-dependency job-max-time)
      (when dependee-failed
        (cl-assert coq-compile-keep-going
                   nil "compilation continues without keep going")
        (coq-par-mark-job-failing job)))
    ;; common part for job where coqdep was successful and where
    ;; coqdep failed (when coq-compile-keep-going)
    (put job 'state 'waiting-dep)
    (if (coq-par-dependencies-ready job)
	(progn
	  (when coq--debug-auto-compilation
	    (message "%s: coqc dependencies finished" (get job 'name)))
          (if (eq (get job 'type) 'file)
              (coq-par-compile-job-maybe job)
            ;; require jobs
            (coq-par-kickoff-queue-maybe job)))
      (when coq--debug-auto-compilation
	(message "%s: wait for %d dependencies"
		 (get job 'name) (get job 'coqc-dependency-count))))))

(defun coq-par-coqc-continuation (process exit-status)
  "Coqc continuation function.
If coqc failed, signal an error or mark the job as 'failed, and
unlock ancestors as appropriate.  If coqc was successful, trigger
the transition 'enqueued-coqc -> 'ready for the job
behind PROCESS."
  (let ((job (process-get process 'coq-compilation-job)))
    (if (eq exit-status 0)
	(progn
	  ;; coqc success
	  (when (get job 'second-stage)
	    (coq-par-second-stage-enqueue job))
	  (coq-par-kickoff-coqc-dependants job t))
      ;; coqc error
      (coq-compile-display-error
       (mapconcat #'identity (process-get process 'coq-process-command) " ")
       (process-get process 'coq-process-output)
       t)
      (if coq-compile-keep-going
	  (progn
	    (coq-par-mark-job-failing job)
	    (coq-par-kickoff-coqc-dependants job nil))
	(signal 'coq-compile-error-coqc
		(get (process-get process 'coq-compilation-job) 'src-file))))))

(defun coq-par-vok-continuation (process exit-status)
  "Vok continuation function.
Nothing to do in case of success. Otherwise display the errors."
  (let ((job (process-get process 'coq-compilation-job)))
    (if (eq exit-status 0)
	;; success - nothing to do
	(when coq--debug-auto-compilation
	  (message "%s: vok finished successfully" (get job 'name)))
      (when coq--debug-auto-compilation
	(message "%s: vok failed" (get job 'name)))
      (coq-compile-display-error
       (mapconcat 'identity (process-get process 'coq-process-command) " ")
       (process-get process 'coq-process-output)
       t)
      ;; don't signal an error or abort other vok processes
      )))

(defun coq-par-vio2vo-continuation (process exit-status)
  "Vio2vo continuation function.
Nothing to do in case of success. Otherwise display the errors."
  (let ((job (process-get process 'coq-compilation-job)))
    (if (eq exit-status 0)
	;; success - nothing to do
	(when coq--debug-auto-compilation
	  (message "%s: vio2vo finished successfully" (get job 'name)))
      (when coq--debug-auto-compilation
	(message "%s: vio2vo failed" (get job 'name)))
      (coq-compile-display-error
       (concat
	"cd "
	(file-name-directory (file-truename (get job 'src-file)))
	"; "
	(mapconcat 'identity (process-get process 'coq-process-command) " "))
       (process-get process 'coq-process-output)
       t)
      ;; don't signal an error or abort other vio2vo processes
      )))


;;; handle Require commands when queue is extended

(defun coq-par-handle-require-list (require-items)
  "Start compilation for the required modules in the car of REQUIRE-ITEMS.
REQUIRE-ITEMS is a list of queue items, eventually to be added to
`proof-action-list', that contains just one require command in
the first element. This function starts the compilation process
for all modules and their dependencies in this require command.

This function creates one require job for the require command,
maintains `coq--last-compilation-job' and adds the delete action
to delete all ancestors to the span of the require command.
Coqdep is directly started on the require job or the job is
enqueued.

This function must be evaluated with the buffer that triggered
the compilation as current, otherwise a wrong `coq-load-path'
might be used.

This function is called synchronously when asserting."
  ;; need a current proof script buffer where we can switch to in a
  ;; sentinel or in a timer function
  (cl-assert (bufferp proof-script-buffer) nil
             "no current proof script buffer when handling require commands")
  (let ((span (caar require-items))
        new-job)
    (when coq--debug-auto-compilation
      (let* ((require-item (car require-items))
             (require-command (mapconcat 'identity (nth 1 require-item) " ")))
        (message "handle require command \"%s\"\nin script buffer %s"
                 require-command proof-script-buffer)))
    (span-add-delete-action
     span
     (lambda () (coq-unlock-all-ancestors-of-span span)))
    ;; create a new require job and maintain coq--last-compilation-job
    (setq new-job
          (coq-par-create-require-job coq-load-path require-items span
                                      proof-script-buffer))
    (when coq--last-compilation-job
      (coq-par-add-queue-dependency coq--last-compilation-job new-job))
    (setq coq--last-compilation-job new-job)
    (when coq--debug-auto-compilation
      (message "%s: this job is the last compilation job now"
               (get coq--last-compilation-job 'name)))
    (coq-par-start-or-enqueue new-job)))
    

(defun coq-par-item-require-predicate (item)
  "Return t if ITEM contains a Require command.
Predicate for `split-list-at-predicate', used to split the new
queue items at each Require command."
  (let ((string (mapconcat #'identity (nth 1 item) " ")))
    (and string
	 (string-match coq-require-command-regexp string))))


(defun coq-par-preprocess-require-commands-internal ()
  "Worker for `coq-par-preprocess-require-commands'.
This function does all the work for
`coq-par-preprocess-require-commands', except for error
reporting.

If `coq-compile-before-require' is non-nil, this function starts
the compilation (if necessary) of the dependencies
ansynchronously in parallel in the background.

If there already is a last compilation job (`coq--last-compilation-job')
then the queue region is extended, while some background
compilation is still running.  In this case I have to preserve the
internal state.  Otherwise the hash of the compilation jobs and
the ancestor hash are reinitialized.

As next action the new queue items are splitted at each Require
command.  The items before the first Require are appended to the
old last compilation job or put back into ‘proof-action-list’.  The
remaining batches of items that each start with a Require are
then processed by `coq-par-handle-require-list', which creates
require jobs as necessary.  Before processing the
first of these batches, buffers are saved with
`coq-compile-save-some-buffers' and an possibly ongoing second stage
compilation is stopped.

Finally, `proof-second-action-list-active' is set if I keep some
queue items because they have to wait for a compilation job.  Then
the maximal number of background compilation jobs is started.
This function is called synchronously when asserting."
  (when coq--debug-auto-compilation
    (message "%d items were added to the queue, scan for require"
	     (length queueitems)))
  (unless coq--last-compilation-job
    (coq-par-init-compilation-hash)
    (coq-init-compile-response-buffer))
  (let ((splitted-items
	 (split-list-at-predicate queueitems
				  'coq-par-item-require-predicate)))
    (if coq--last-compilation-job
	(progn
	  (put coq--last-compilation-job 'queueitems
	       (nconc (get coq--last-compilation-job 'queueitems)
		      (car splitted-items)))
	  (setq queueitems nil)
	  (message "attach first %s items to job %s"
		   (length (car splitted-items))
		   (get coq--last-compilation-job 'name)))
      (setq queueitems (car splitted-items))
      (when coq--debug-auto-compilation
	(message "attach first %s items directly to queue"
		 (length (car splitted-items)))))
    ;; XXX handle external compilation here, compile everything
    ;; with one command, use compilation-finish-functions to get
    ;; notification
    (when (cdr splitted-items)
      (when coq--par-second-stage-delay-timer
	(cancel-timer coq--par-second-stage-delay-timer)
	(setq coq--par-second-stage-delay-timer nil))
      (when coq--par-second-stage-in-progress
	(cl-assert (not coq--last-compilation-job)
		nil "normal compilation and second stage in parallel 2")
	;; there are only second stage background processes
	(coq-par-kill-all-processes)
	(setq coq--par-second-stage-in-progress nil))
      ;; save buffers before invoking the first coqdep
      (coq-compile-save-some-buffers)
      (dolist (require-items (cdr splitted-items))
	(coq-par-handle-require-list require-items)))
    (when coq--last-compilation-job
      (setq proof-second-action-list-active t))
    (coq-par-start-jobs-until-full)
    (when coq--debug-auto-compilation
      (if coq--last-compilation-job
	  (message "return control, waiting for background jobs")
	(message "return control, no background jobs")))))

(defun coq-par-preprocess-require-commands ()
  "Coq function for `proof-shell-extend-queue-hook' doing parallel compilation.
If `coq-compile-before-require' is non-nil, this function starts
the compilation (if necessary) of the dependencies
ansynchronously in parallel in the background.  This function only
does the error checking/reporting for
`coq-par-preprocess-require-commands-internal', which does all
the work. This function is called synchronously when asserting."
  (when coq-compile-before-require
    (condition-case err
	(coq-par-preprocess-require-commands-internal)
      (coq-compile-error
       (coq-par-emergency-cleanup)
       (message "%s %s" (get (car err) 'error-message) (cdr err)))
      (coq-unclassifiable-version
       (coq-par-emergency-cleanup)
       (if (equal (cdr err) "trunk")
	   (message
	    (concat "your Coq version \"trunk\" is too unspecific for "
		    "Proof General; please customize coq-pinned-version"))
	 (message "%s \"%s\"; consider customizing coq-pinned-version"
		  (get (car err) 'error-message) (cdr err))))
      (file-error
       (coq-par-emergency-cleanup)
       (message "Error: %s" (mapconcat 'identity (cdr err) ": ")))
      (error
       (message "Unexpected error during parallel compilation: %s"
		err)
       (coq-par-emergency-cleanup)
       (signal (car err) (cdr err))))))


(provide 'coq-par-compile)


;;   Local Variables: ***
;;   coding: utf-8 ***
;;   End: ***

;;; coq-par-compile.el ends here

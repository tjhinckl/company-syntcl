;;; company-netlist.el --- company-mode completion backend for NetlistComplete  -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2011, 2013-2016  Free Software Foundation, Inc.

;; Author: Frank Creed
;; email:  francis.creed@intel.com
;; Package-Requires: ((emacs "25.1") (company "0.9.3") (s "1.11.0") (ht "2.0"))

;; This file is *not* part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; this company backend provides completion for hierarchy, cells, ports, nets,
;; based upon a netlist style hierarchy

;; this is based upon Chris Heithoffs's NetlistComplete Vim plugin
;; /nfs/pdx/home/cbheitho/MyVimPlugins/NetlistComplete

;; Populating the completion files

;; ** 2018 feb update

;; NetlistComplete files are JSON files auto-generated during ICCDP2 build and are in:
;; $WARD/dp/user_scripts/NetlistComplete/
;; dictionaries of lists -> hash table
;; ./cells.json
;; ./hier_cells.json     key:"block" val1: {key2: "hier-ref" val2:"ref-cell"}
;; ./nets.json
;; ./ports.json
;; dictionary of dictionaries -> nested hash-table
;; ./refs.json

;;; Prerequisites

;; ;; First of all, you need to put this command at the top of your ~/.emacs or ~/.emacs.d/init.el file
;; (package-initialize)

;; ;; Next, set up some variables to find package archives
;; (setq url-proxy-services
;;    '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;      ("http" . "proxy-us.intel.com:911")
;;      ("https" . "proxy-us.intel.com:912")))

;; (setq package-archives
;;   '(("gnu" . "http://elpa.gnu.org/packages/")
;;     ("marmalade" . "https://marmalade-repo.org/packages/")
;;     ("melpa" . "http://melpa.org/packages/")))

;; Next, in order to use this, you will need to install the "company" completion package from MELPA
;; M-x package-install RET company RET

;; You also need to install the ht package (hash-table functions)
;; M-x package-install RET ht RET

;; You also need to install the s package (string functions)
;; M-x package-install RET s RET

;; You also need to install the json package
;; M-x package-install RET json RET

;; Then, copy these 3 files to your Emacs load path:
;; cp /nfs/pdx/home/fcreed/.emacs.d/my-elisp/company-netlist.el  ~/<your_emacs_load_path>
;; cp /nfs/pdx/home/fcreed/.emacs.d/my-elisp/company-syntcl.el   ~/<your_emacs_load_path>
;; cp /nfs/pdx/home/fcreed/.emacs.d/my-elisp/make-tcl-abbrevs.el ~/<your_emacs_load_path>

;; and add the following lines to your Emacs init file:
;; UPDATE: 2018jun19 - following 2 lines are NOT necesssary (`company-netlist', `company-syntcl' functions autoloaded)
;; (require 'company-netlist)
;; (require 'company-syntcl)

;; (set-default 'abbrev-mode t)
;; (require 'make-tcl-abbrevs)

;; (add-hook 'tcl-mode-hook
;; 	  (lambda()
;; 	    (define-key tcl-mode-map [delete] 'delete-forward-char)
;; 	    (define-key tcl-mode-map "\C-ch"  'company-netlist--hier)
;; 	    (define-key tcl-mode-map "\C-cc"  'company-netlist--cells)
;; 	    (define-key tcl-mode-map "\C-cn"  'company-netlist--nets)
;; 	    (define-key tcl-mode-map "\C-cp"  'company-netlist--ports)
;; 	    (define-key tcl-mode-map "\C-ct"  'company-syntcl--tcl)
;; 	    (add-to-list (make-local-variable 'company-backends)
;; 			 '(company-netlist company-syntcl company-etags))
;; 	    (set (make-local-variable 'company-idle-delay) company-netlist--idle-delay)
;;          (make-tcl-abbrevs)))

;;; Sample Keybindings

;; <cntl>-c h      **Hierarchy** based auto-complete mode.
;;                 These are the hierarchical names of cells starting with the $block toplevel.
;; <cntl>-c c      **Cell** based auto-complete mode.
;;                 These are the child cells of the $block, or the child cells of the previous level of hierarchy
;;                 if slash hierarchy delimiters are used.
;; <cntl>-c p      **Port** based auto-complete mode.
;;                 These are the ports of $block, or the ports of the previous level of hierarchy if
;;                 slash hierarchy delimiters are used.
;; <cntl>-c n      **Net** based auto-complete mode.
;;                 These are the nets of $block, or the nets of the previous level of hierarchy if
;;                 slash hierarchy delimiters are used.

;; <cntl>-c t      Synopsys Tcl completion

;; <alt>-v         scroll previous page of completions
;; <cntl>-v        scroll next page of completions
;; <cntl>-s        search forward in completions
;; <cntl>-r        search reverse in completions
;; <cntl>-<alt>-s  narrow completion candidates to search string
;; <cntl>-a        prints out completion candidates below current line

;;; Customization options

;; there are some customize options available to control how Netlist Complete mode works
;; type "M-x customize-group RET company-netlist RET" to get list of user-specified options

;;; Change log
;; 2017-12-19 fcreed cleanup comment strings, bump version number
;; 2017-12-20 fcreed add annotation for "cells" completion: '"cell"  (ref_cell)'
;; 2017-12-21 fcreed use hierarchy browser ref-map hash table instead of *-get-ref function
;; 2017-12-27 fcreed code/comment cleanup
;; 2018-01-10 fcreed corrected logic in *--make-refmap-hash to only generate *-refmap.el if needed
;;                   only load hier-browser-refmap hash-table if not already loaded
;; 2018-01-24 fcreed added source of downloaded functions to part of doc string
;; 2018-02-05 fcreed fixed logic to test whether *-refmap.el needs to be generated
;;                   from: https://emacs.stackexchange.com/questions/11035/compare-file-modification-time-stamps
;; 2018-02-09 fcreed Chris changed NetlistComplete source files to JSON files in $WARD/dp/user_scripts/NetlistComplete
;;                   re-wrote functions to use JSON info
;; 2018-02-20 fcreed added error checking for existence of *.json files
;; 2018-02-22 fcreed added custom variable `company-netlist--force-read-json' for cases when JSON updated via git
;;                   added 'group' param to defcustom variables
;; 2018-05-03 fcreed updated comments on NetlistComplete source files, commented out obsolete functions
;;                   added C-a keybinding to insert completion candidates after point
;; 2018-05-08 fcreed only use 'defvar' once to create global var, use setq to assign value
;; 2018-05-15 fcreed code/comment cleanup
;;                   sort completion candidates alphabetically (a->z)
;; 2018-06-12 fcreed add 'Package-Requires' to identify dependencies
;;                   add autoload cookie to `company-netlist' (h/t to Troy Hinckley)
;;                   simplify conditionals from '(not (null var))' to 'var'
;; 2018-06-18 fcreed rewrote *-read-json-files, add test for embedded tabs (breaks emacs 26 JSON parser)
;; 2018-06-26 fcreed add "begin/end read json" messages to *--read-json-file

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'ht)            ; hash table functions
(require 's)             ; string functions
(require 'json)		 ; to read/decode JSON data

(defgroup company-netlist nil
  "Completion backend for NetlistComplete."
  :group 'company-netlist)

(defvar company-netlist--version "2.3")

;; NOTE: may not want to restrict to tcl mode (adding text/emacs-lisp modes for debug
(defvar company-netlist-modes '(tcl-mode)
  "Major modes which nc (NetlistComplete) may complete.")

(defvar company-netlist-ward "")
(if (getenv "WARD")
    (setq company-netlist-ward (getenv "WARD"))
  (setq company-netlist-ward "/nfs/pdx/disks/sdg74_0819/work/fcreed/sprspscc_tr_ww45.4") ; default
  )
(defvar company-netlist-top-blk "")
(if (getenv "block")
    (setq company-netlist-top-blk (getenv "block"))
  (setq company-netlist-top-blk "sprspxcc") ; default
  )

;; if running $WARD/dp/user_scripts/WriteNetlistInfo.tcl manually, result will be in $WARD/NetlistComplete
;; else, they will be in $WARD/dp/user_scripts/NetlistComplete (under git control)
(defvar company-netlist-dir "")
(if (file-directory-p (expand-file-name "NetlistComplete" company-netlist-ward))
    (setq company-netlist-dir (expand-file-name "NetlistComplete" company-netlist-ward))
  (if (file-directory-p (expand-file-name (concat "dp/user_scripts/" "NetlistComplete") company-netlist-ward))
      (setq company-netlist-dir (expand-file-name (concat "dp/user_scripts/" "NetlistComplete") company-netlist-ward))
    (setq company-netlist-dir "/tmp"))   ; when not starting from valid WARD or prior to setproj/setup/populate
  )

(defcustom company-netlist-hier-sep "/"
  "hierarchical separator for NetlistComplete (NC) mode (default: \"/\")
can set to alternate value (i.e. \".\")"
  :type 'string
  :group 'company-netlist)

(defcustom company-netlist--debug nil
  "enable to see additional info in *Messages* buffer"
  :type 'boolean
  :group 'company-netlist)

(defcustom company-netlist--prefix-start nil
  "character start position for candidate matching (default: \"off\" means anywhere in string)
set to \"on\" (t) to have candidates *only* match from beginning of entered text"
  :type 'boolean
  :group 'company-netlist)

(defcustom company-netlist--ignore-case nil
  "switch to control case-sensitive matching (default: \"off\" means case-sensitive matches)
set to \"on\" (t) to ignore case for candidate matches"
  :type 'boolean
  :group 'company-netlist)

(defcustom company-netlist--idle-delay nil
  "controls delay for idle matching
default (\"nil\") means *NO* idle triggering of NC completion.
This means you must explicitly trigger netlist completion.
Set a delay (in seconds) to auto-trigger Netlist completion.
NOTE: better to keep at default (nil) to avoid wrong completion type"
  :type '(choice
	  (const :tag "never (nil)" nil)
	  (number :tag "seconds"))
  :group 'company-netlist)

(defcustom company-netlist--force-read-json nil
  "force reading JSON files (default \"nil\")
set to \"on\" (t) to force (i.e. if JSON files change on updated database)"
  :type 'boolean
  :group 'company-netlist)

(defvar company-netlist--type nil)  ; default, set in interactive functions bound to keys below

(defvar company-netlist--cmd-max-len 0)  ; used to align annotations

;; make byte compiler happy
(defvar company-netlist-cells-hash nil)
(defvar company-netlist-ports-hash nil)
(defvar company-netlist-nets-hash nil)
(defvar company-netlist-refs-hash nil)	; nested hash table: key:"block" val1: {key2: "hier-ref" val2:"ref-cell"}
(defvar company-netlist-hier_cells-hash nil) ; contains all hierarchical references
(defvar my-json-hash nil)

(defun company-netlist--hier ()
  "activate Netlist completion for hierarchy"
  ;; set interactive so it can be bound to key
  (interactive)
  (setq company-netlist--type "hier")
  (company-mode 1)
  (company-netlist 'interactive))

(defun company-netlist--cells ()
  "activate Netlist completion for cells"
  ;; set interactive so it can be bound to key
  (interactive)
  (setq company-netlist--type "cells")
  (company-mode 1)
  (company-netlist 'interactive))

(defun company-netlist--ports ()
  "activate Netlist completion for ports"
  ;; set interactive so it can be bound to key
  (interactive)
  (setq company-netlist--type "ports")
  (company-mode 1)
  (company-netlist 'interactive))

(defun company-netlist--nets ()
  "activate Netlist completion for nets"
  ;; set interactive so it can be bound to key
  (interactive)
  (setq company-netlist--type "nets")
  (company-mode 1)
  (company-netlist 'interactive))


;; prefix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE to self: do I need to limit prefix to string after last '/'???
(defun company-netlist--prefix ()
  "grabs completion string prefix using `company-grab-symbol.
NOTE: returns string"
  (let ((prfx (company-grab-symbol)))
    ;; (if (not (null company-netlist--debug))
    (if company-netlist--debug
	(message "`company-netlist--prefix' grabbed <%s>" prfx))
    (when prfx
      ;; Completion candidates for annotations don't include '@'.
      (cond
       ((eq ?@ (string-to-char prfx)) (setq prfx (substring prfx 1))) ; NOTE: string-to-char returns first char of string
       ((eq ?: (string-to-char prfx)) (setq prfx (substring prfx 1))) ; itar command options
       ((string-match ":" prfx)       (setq prfx (substring prfx (+ 1 (string-match ":" prfx))))) ; take everything to right of ":" as prfx
       )
      prfx)))


;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: this function is used by hier-browser.el and tcl-extra-highlighting.el !!
(defun company-netlist-file-to-list (filename)
  "reads 'filename' into temp buffer
splits each line into space-delimited list
i.e. 'foo bar bar1 bar2' -> (\"foo\" \"bar\" \"bar1\" \"bar2\")
NOTE: returns list of lists
  downloaded from: https://stackoverflow.com/questions/28043763/elisp-read-file-into-list-of-lists#"
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((list '()))
      (while (not (eobp))
        (let ((beg (point)))
          (move-end-of-line nil)
          (push (split-string (buffer-substring-no-properties beg (point)) " ") list)
          (forward-char)))
      (nreverse list))))

(defun company-netlist--indented-list (my-list)
  "appends list on newline below point"
  (let (indented-list)
    (dolist (item my-list indented-list)
      (setq indented-list (cons (concat "     " item "\n") indented-list)))
    (newline)
    (insert (message "%s" indented-list))))

(defun company-netlist--insert-candidates (cands)
  "appends candidates on newline below point"
  (newline)
  (dolist (item cands)
    (insert (concat "    " item "\n")))
  )


;; required inputs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; all netlistcomplete info is now stored in following JSON files:
;; dictionary of lists -> (hash-table)
;;   <netlist-complete-dir>/cells.json
;;   <netlist-complete-dir>/ports.json
;;   <netlist-complete-dir>/nets.json
;;   <netlist-complete-dir>/hier_cells.json
;; dictionary of dictionaries -> (nested hash-table)
;;   <netlist-complete-dir>/refs.json

(defun company-netlist--read-json-files ()
  "make hash tables for NetlistComplete data
All NC data is contained in the following 5 JSON files
<netlist-complete-dir>/cells.json, ports.json, nets.json, hier_cells.json (hash-table)
<netlist-complete-dir>/refs.json (nested hash-table)
NOTE: only read JSON file if *-<type>-hash variable NOT already set
"
  (let* ((json-object-type 'hash-table)
	 (json-array-type  'list)
	 (json-key-type    'string))
    (setq my-json-hash '("cells"
			 "ports"
			 "nets"
			 "refs"
			 "hier_cells"))
    (message "company-netlist: Begin reading JSON files")
    (dolist (fl my-json-hash)
      (if company-netlist--debug
	  (message "Processing JSON file: %s for var: %s" (concat fl ".json") (concat "company-netlist-" fl "-hash")))
      (if (or (null (symbol-value (intern-soft (concat "company-netlist-" fl "-hash"))))
	      company-netlist--force-read-json)
	  (if (file-exists-p (expand-file-name (concat fl ".json") company-netlist-dir))
	      (if (version< "26.1" emacs-version)
		  (set (intern-soft (concat "company-netlist-" fl "-hash")) (json-read-file (expand-file-name (concat fl ".json") company-netlist-dir)))
		;; else, need to test for "broken" JSON constructs in emacs 26.1 (i.e. embedded tabs)
		(if (string-blank-p (shell-command-to-string (concat "grep -P '\".*\t.*\"' " (expand-file-name (concat fl ".json") company-netlist-dir))))
		    (set (intern-soft (concat "company-netlist-" fl "-hash")) (json-read-file (expand-file-name (concat fl ".json") company-netlist-dir)))
		  ;; else, need to fix!
		  (message "File %s has embedded tabs, which breaks Emacs %s JSON parser" (concat fl ".json") emacs-version)
		  (with-temp-buffer
		    (insert-file-contents (expand-file-name (concat fl ".json") company-netlist-dir))
		    (goto-char (point-min))
		    (while (search-forward "	" nil t) ; emacs 26 json parser does not like embedded tabs!
		      (replace-match " " nil t))
		    ;; (write-region (point-min) (point-max) (expand-file-name (concat fl "_mod" ".json") company-netlist-dir)) ; write out modified file
		    (goto-char (point-min))
		    (set (intern-soft (concat "company-netlist-" fl "-hash")) (json-read))
		    )
		  ;; (set (intern-soft (concat "company-netlist-" fl "-hash")) (json-read-file (expand-file-name (concat fl "_mod" ".json") company-netlist-dir)))
		  ))
	    (message "File %s does not exist in dir %s. Cannot perform completions." (concat fl ".json") company-netlist-dir))
	(message "var %s already exists" (intern-soft (concat "company-netlist-" fl "-hash"))))
      )					; end dolist
    (message "company-netlist: end reading JSON files")
    ))


;; candidate parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-netlist--hier-candidates (blk prfx)
  "AutoComplete function for hierarchy
NOTE: returns list of hierarchy candidates"
  (if company-netlist--debug
      (message "`company-netlist--hier-candidates' gets <%s> <%s>" blk prfx))
  (let* ((case-fold-search company-netlist--ignore-case)
	 (hier-obj-list (ht-get company-netlist-hier_cells-hash blk))
	 (hier-complete-opts '()))
    (if company-netlist--debug
	(message "`company-netlist--hier' case-fold-search = <%s>\n`company-netlist--prefix-start = <%s>" case-fold-search company-netlist--prefix-start))
    (dolist (element hier-obj-list hier-complete-opts)
      (if company-netlist--prefix-start
	  (if (eq 0 (string-match prfx element))
	      (setq hier-complete-opts (cons element hier-complete-opts)))
	(if (string-match prfx element)
	    (setq hier-complete-opts (cons element hier-complete-opts)))))
    (delete-dups hier-complete-opts)))

(defun company-netlist--other-candidates (blk prfx compl-typ)
  "AutoComplete function for cells, ports, nets
NOTE: returns list of [cell/port/net] candidates"
  (if company-netlist--debug
      (message "`company-netlist--other-candidates' gets blk=<%s> prefix=<%s> type=<%s>" blk prfx compl-typ))
  (let ((case-fold-search company-netlist--ignore-case)
	(ref-name (ht-get* company-netlist-refs-hash company-netlist-top-blk blk))
	(obj-complete-opts '())
	(obj-list))
    (if company-netlist--debug
	(message "`company-netlist--other-candidates' has case-fold-search = <%s>\n`company-netlist--other-candidates' has `company-netlist--prefix-start set to <%s>\n`company-netlist-refs-hash' returns <%s>" case-fold-search company-netlist--prefix-start ref-name))
    (if (null ref-name)
	(message (concat "No reference name found for " blk " in " (expand-file-name "refs.json" company-netlist-dir)))
      ;; else ...
      ;; intern-soft -> converts string to symbol
      ;; symbol-value -> get the value of symbol (i.e. hash-table)
      ;; ht-get -> returns value for hash-table-key=blk (whew!)
      (setq obj-list (ht-get (symbol-value (intern-soft (concat "company-netlist-" compl-typ "-hash"))) ref-name))
      (dolist (element obj-list obj-complete-opts)
	(if company-netlist--prefix-start
	    (progn
	      (if (eq 0 (string-match prfx element))
		  (progn
		    (if (> (+ (length element) (length blk)) company-netlist--cmd-max-len)
			(setq company-netlist--cmd-max-len (+ (length element) (length blk))))
		    (setq obj-complete-opts (cons (concat blk "/" element) obj-complete-opts)))))
	  (if (string-match prfx element)
	      (progn
		(if (> (+ (length element) (length blk)) company-netlist--cmd-max-len)
		    (setq company-netlist--cmd-max-len (+ (length element) (length blk))))
		(setq obj-complete-opts (cons (concat blk "/" element) obj-complete-opts)))
	    ))))
    (delete-dups obj-complete-opts)))

(defun company-netlist--candidates (prefix obj-type)
  "Processes auto-complete prefix to generate completion candidates.
Everything before right-most hierarchy separator (default \"/\") is
processed as 'block-name', which is parent/owner of the completion.
AutoComplete function for hierarchy, cells, ports, nets
(determined by 'company-netlist--type' variable)
NOTE: returns list of completion candidates"
  (if company-netlist--debug
      (message "`company-netlist--candidates' args <%s> <%s>" prefix obj-type))
  (let ((company-netlist--blk)
	(company-netlist--prfx prefix)
	(obj-complete-opts '())
	(indx (string-match company-netlist-hier-sep prefix)))
    (setq company-netlist--cmd-max-len 0)  ; reset here
    (if (null indx)   ; no hier-sep
	(setq company-netlist--blk company-netlist-top-blk)
      (while (numberp indx)  ; at least 1 hier-sep
	(setq company-netlist--blk  (substring prefix 0 indx))
	(setq company-netlist--prfx (substring prefix (+ indx 1)))
	(setq indx (string-match company-netlist-hier-sep prefix (+ indx 1)))))
    (if company-netlist--debug
	(message "`company-netlist--candidates' passing blk=<%s> prfx=<%s> typ=<%s>" company-netlist--blk company-netlist--prfx obj-type))
    (if (string-equal obj-type "hier")
	(setq obj-complete-opts (company-netlist--hier-candidates company-netlist--blk company-netlist--prfx))
      (setq obj-complete-opts (company-netlist--other-candidates company-netlist--blk company-netlist--prfx obj-type)))
    (sort obj-complete-opts 'string<)))


;; annotation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-netlist--annotation (candidate)
  "annotation for CELL completion candidates
source is ./TclComplete/$block/ref_map.txt
NOTE: returns annotation *string* for each candidate"
  (if company-netlist--debug
      (message "`company-netlist--annotation' gets <%s>\ncmd-max-len: %s" candidate company-netlist--cmd-max-len))
  (let ((anno "")
	(pad-len (- (+ 5 company-netlist--cmd-max-len) (length candidate))))
    (if (string-equal "cells" company-netlist--type)
	(setq anno (concat (s-pad-left pad-len " " "(") (ht-get* company-netlist-refs-hash company-netlist-top-blk candidate) ")")))
    anno))


;; setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-netlist--setup ()
  "Instructions to setup Netlist and Tcl Completion:
* First of all, you need to put this command at the top of your ~/.emacs or ~/.emacs.d/init.el file
(package-initialize)

* Next, set up some variables to find package archives
(setq url-proxy-services
      '((\"no_proxy\" . \"^\\(localhost\\|10.*\\)\")
	(\"http\" . \"proxy-us.intel.com:911\")
	(\"https\" . \"proxy-us.intel.com:911\")))

(setq package-archives
      '((\"gnu\" . \"http://elpa.gnu.org/packages/\")
	(\"marmalade\" . \"http://marmalade-repo.org/packages/\")
	(\"melpa\" . \"http://melpa.org/packages/\")))

* Next, in order to use this, you will need to install the \"company\" completion package from MELPA
M-x package-install RET company RET

* You also need to install the ht package (hash-table functions)
M-x package-install RET ht RET

* You also need to install the s package (string functions)
M-x package-install RET s RET

* You also need to install the json package
M-x package-install RET json RET

* Then, copy these 3 files to your Emacs load path:
cp /nfs/pdx/home/fcreed/.emacs.d/my-elisp/company-netlist.el  ~/<your_emacs_load_path>
cp /nfs/pdx/home/fcreed/.emacs.d/my-elisp/company-syntcl.el   ~/<your_emacs_load_path>
cp /nfs/pdx/home/fcreed/.emacs.d/my-elisp/make-tcl-abbrevs.el ~/<your_emacs_load_path>

(set-default 'abbrev-mode t)
(require 'make-tcl-abbrevs)

(add-hook 'tcl-mode-hook
	  (lambda()
	    (define-key tcl-mode-map [delete] 'delete-forward-char)
	    (define-key tcl-mode-map \"\\C-ch\"  'company-netlist--hier)
	    (define-key tcl-mode-map \"\\C-cc\"  'company-netlist--cells)
	    (define-key tcl-mode-map \"\\C-cn\"  'company-netlist--nets)
	    (define-key tcl-mode-map \"\\C-cp\"  'company-netlist--ports)
	    (define-key tcl-mode-map \"\\C-ct\"  'company-syntcl--tcl)
	    (add-to-list (make-local-variable 'company-backends)
			 '(company-netlist company-syntcl company-etags))
	    (set (make-local-variable 'company-idle-delay) company-netlist--idle-delay)
	    (make-tcl-abbrevs)))
"
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun company-netlist (command &optional arg &rest ignored)
  "`company-mode' completion backend for NetlistComplete.
this company backend provides completion for hierarchy, cells, ports, nets,
based upon a netlist style hierarchy

this is based upon Chris Heithoffs's NetlistComplete Vim plugin
/nfs/pdx/home/cbheitho/MyVimPlugins/NetlistComplete

** Sample Keybindings **

 <cntl>-c h    Hierarchy based auto-complete mode.
                  These are the hierarchical names of cells starting with the $block toplevel.
 <cntl>-c c    Cell based auto-complete mode.
                  These are the child cells of the $block, or the child cells of the previous level of hierarchy
                  if slash hierarchy delimiters are used.
 <cntl>-c p    Port based auto-complete mode.
                  These are the ports of $block, or the ports of the previous level of hierarchy if
                  slash hierarchy delimiters are used.
 <cntl>-c n    Net based auto-complete mode.
                  These are the nets of $block, or the nets of the previous level of hierarchy if
                  slash hierarchy delimiters are used.

 <alt>-v          scroll previous page of completions
 <cntl>-v         scroll next page of completions
 <cntl>-s         search forward in completions
 <cntl>-r         search reverse in completions
 <cntl>-<alt>-s   narrow completion candidates to search string
 <cntl>-a         print out completion candidates below current line

** Customization options **

there are some customize options available to control how Netlist Complete mode works

type \"M-x customize-group RET company-netlist RET\" to get list of user-specified options
"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-netlist))
    (init (when (memq major-mode company-netlist-modes)
	    (company-netlist--read-json-files))) ; read all JSON files into variables
    (prefix (and (memq major-mode company-netlist-modes)
                 (or (company-netlist--prefix) 'stop)))
    (candidates (company-netlist--candidates arg company-netlist--type))
    ;; (meta       (company-netlist--meta arg))
    (annotation (company-netlist--annotation arg))
    (no-cache t)
    (ignore-case company-netlist--ignore-case)
    (sorted t)
    ))

(provide 'company-netlist)
;;; company-netlist.el ends here

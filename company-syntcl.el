;;; company-syntcl.el --- company-mode completion backend for Synopsys Tcl  -*- lexical-binding: t -*-

;; Copyright (C) 2009, 2011, 2013-2016  Free Software Foundation, Inc.

;; Author: Frank Creed
;; Email:  francis.creed@intel.com
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

;; this company backend provides completion for synopsys tcl

;; this is based upon Chris Heithoffs's TclComplete Vim plugin
;; /nfs/pdx/home/cbheitho/MyVimPlugins/TclComplete

;; Populating the completion files

;; Because there can be different versions of Tcl, and also many
;; possible procs that may vary from project to project, it is
;; recommended to open up your Tcl shell (or icc2_shell or pt_shell..)
;; and source the dp/user_scripts/WriteTclCompleteFiles.tcl file.

;; This will create a directory $WARD/TclComplete and populate it with
;; .json files representation the data structures (lists, dicts, dicts of list, etc)
;; for all the completion options.
;; JSON format is not Vim specific, so similar plugins for other text
;; editors can use these.

;;  (NOTE:  This expects user to be an Intel back-end engineer who works
;;   in a back-end environment, where the $WARD is your work area. If
;;   you need this changed, then modify the tcl script or fake it out
;;   by defining an environment variable called WARD.)

;; this file will read the JSON files below and store the results in `tcl-*-hash variables'

;; lists
;;   <tcl-complete-dir>/commands.json                    command
;;   <tcl-complete-dir>/designs.json                     subfc/ss/partition
;;   <tcl-complete-dir>/g_vars.json                      g_variables but no arrays (includes "G_AON_CONNECT_VIA_TABLE(" )
;;   <tcl-complete-dir>/g_var_arrays.json                g_variables with arrays
;;   <tcl-complete-dir>/iccpp.json                       iccpp parameters
;;   <tcl-complete-dir>/techfile_types.json              types
;;   <tcl-complete-dir>/app_vars.json                    app variables
;;   <tcl-complete-dir>/regexp_char_classes.json         regexp character classes
;;   <tcl-complete-dir>/packages.json                    packages
;;
;; dictionary of lists -> (hash-table)                   key                     value
;;   <tcl-complete-dir>/descriptions.json                command                 description of command
;;   <tcl-complete-dir>/options.json                     command                 list of options
;;   <tcl-complete-dir>/app_options.json                 option                  type:default
;;   <tcl-complete-dir>/environment.json                 env_var                 value
;;   <tcl-complete-dir>/iccpp_dict.json                  iccpp param             default
;;   <tcl-complete-dir>/techfile_layer_dict.json         techfile_type           layer options
;;
;; dictionary of dictionaries -> (nested hash-table)     key                     value
;;   <tcl-complete-dir>/details.json                     [command][option]       description of option
;;   <tcl-complete-dir>/attributes.json                  [obj_class][attr_name]  choices
;;   <tcl-complete-dir>/techfile_attr_dict.json          ["type":"layer"]        choices

;; Special categories of completion:

;; - variable names starting with $
;; - G_variables
;; - attributes in the get_attribute/set_attribute commands
;; - expr functions
;; - command ensembles (dict, string, package, etc)
;; - package require <your_list_of_packages_here>
;; - iccpp parameters with iccpp_com::get_param and iccpp_com::set_param
;; - tech::get_techfile_info autocompletion
;; - getenv completion
;; - get_xxx -design completion
;; - smarter attribute completion to derive the object class from get_cells, get_nets, etc.


;;; Prerequisites

;; First of all, you need to put this command at the top of your ~/.emacs or ~/.emacs.d/init.el file
;; (package-initialize)

;; Next, set up some variables to find package archives
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
;;    (lambda()
;;      (define-key tcl-mode-map [delete] 'delete-forward-char)
;;      (define-key tcl-mode-map "\C-ch"  'company-netlist--hier)
;;      (define-key tcl-mode-map "\C-cc"  'company-netlist--cells)
;;      (define-key tcl-mode-map "\C-cn"  'company-netlist--nets)
;;      (define-key tcl-mode-map "\C-cp"  'company-netlist--ports)
;;          (define-key tcl-mode-map "\C-ct"  'company-syntcl--tcl)
;;      (add-to-list (make-local-variable 'company-backends)
;;           '(company-netlist company-syntcl company-etags))
;;      (set (make-local-variable 'company-idle-delay) company-netlist--idle-delay)
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
;; <cntl>-a        print out completion candidates below current line

;;; Customization options

;; there are some customize options available to control how Tcl Complete mode works
;;
;; type "M-x customize-group RET company-syntcl RET" to get list of user-specified options

;;; Change log

;; 2017-12-19 fcreed clean up comment strings, bump version
;;                   replace 'string=' (compiled Elisp)  with 'string-equal' (C code)
;; 2017-12-27 fcreed code/comment cleanup
;; 2018-01-10 fcreed changed *--prefix to account for '[' being added to tcl 'symbol' syntax table to enable tmux
;;                   ... need to strip '[' from `prefix' before further processing
;;                   only load tcl-*.el files if underlying variables are undefined (speedup repeated tcl completion calls)
;; 2018-02-05 fcreed fixed logic to test whether *-refmap.el needs to be generated
;;                   from: https://emacs.stackexchange.com/questions/11035/compare-file-modification-time-stamps
;; 2018-02-06 fcreed TclComplete moved from dp/inputs to dp/user_scripts per request from Carl & Jon
;;                   refs/heads/TR commit: 7ac9ad5  Moved NetlistComplete and TclComplete from dp/inputs to dp/user_scripts
;; 2018-02-07 fcreed fixed hard-coded references to 'dp/inputs' to use company-syntcl-dir
;; 2018-02-09 fcreed clean up doc strings
;; 2018-02-22 fcreed added group 'company-syntcl' to defcustom variables
;; 2018-05-07 fcreed Chris changed WriteTclCompleteFiles.tcl to output JSON files, AND he added more completion types
;;                   changed *--gvars to use *-gvars-hash, *-gvar-arrays-hash
;;                   created generic *-get-candidate-list function to generate candidate lists (reduce number of unique cand funcs)
;;                   major re-write of *--prefix for additional completion types
;; 2018-05-15 fcreed code cleanup, tweak comments/doc-strings
;;                   add function `company-syntcl--get-xref' - wrapper to `xref-find-definitions' for navigating rdt-tag file
;; 2018-06-12 fcreed add 'Package-Requires' to identify dependencies
;;                   add autoload cookie to `company-syntcl' (h/t to Troy Hinckley)
;;                   simplify conditionals from '(not (null var))' to 'var'
;; 2018-06-18 fcreed rewrote *--read-json-file, added checks for embedded tags (breaks emacs 26 JSON parser)
;; 2018-06-26 fcreed add "begin/end read json" messages to *--read-json-file
;; 2018-06-28 fcreed Chris added "app_vars.json", changed more '(not (null <expr>' to '<expr>'
;;                   fixed `company-syntcl--scan-buffer-for-var-names'
;;                   fixed `company-syntcl--techfile-candidates'
;; 2018-07-17 fcreed modified to match Chris' (major) 7/11/2018 changes
;;                   added tcl-*-funcs vars, to parse list instead of long regexes
;;                   modified `company-syntcl--scan-buffer-for-var-names' to include 'proc' arg and 'scan|lassign' var parsing,
;;                      added arg 'var-type' to process array variables names separately
;;                   modified `company-syntcl--prefix' - do not set `*--attr-flag', add more `*--type' options
;;                   major changes to `company-syntcl--attr-candidates'
;;                   added `company-syntcl--ask-for-attr-class'
;;                   added options to `company-syntcl--get-candidate-list', `company-syntcl--candidates'
;; 2018-09-19 fcreed when changing from indentation tabs to spaces, forgot to change explicit tab char in *-read-json :-(
;; 2018-10-04 fcreed remove obsolete variable `company-syntcl--force-gen-file'
;; 2018-10-08 fcreed add support for track pattern completion (`tcl-track-pattern-list')
;;                   add support for gui_* completions (color,pattern,line_style, symbol_type, type)
;;                   new function `company-syntcl--gui-candidates'

;;; Code:

(require 'company)
(require 'cl-lib)
(require 'ht)     ; hash table functions
(require 's)      ; string functions
(require 'json)   ; read/decode JSON data

(defgroup company-syntcl nil
  "Completion backend for Tclcomplete."
  :group 'company-syntcl)

(defvar company-syntcl--version "3.0")

;; NOTE: may not want to restrict to tcl mode (add text/emacs-lisp modes for debug??)
(defvar company-syntcl-modes '(tcl-mode)
  "Major modes which Tclcomplete may complete.")

(defvar company-syntcl-ward (getenv "WARD"))

;; if running $WARD/dp/user_scripts/WriteTclCompleteFiles.tcl manually, result will be in $WARD/TclComplete
;; else, they will be in $WARD/dp/user_scripts/TclComplete (under git control)
(defvar company-syntcl-dir
  (cond ((file-directory-p (expand-file-name "TclComplete" company-syntcl-ward))
         (expand-file-name "TclComplete" company-syntcl-ward))
        ((file-directory-p (expand-file-name (concat "dp/user_scripts/" "TclComplete") company-syntcl-ward))
         (expand-file-name (concat "dp/user_scripts/" "TclComplete") company-syntcl-ward))
        (t "/tmp")))

(defcustom company-syntcl--prefix-start t
  "character start position for candidate matching (default: \"on\" start match from string beginning)
set to \"off\" to match anywhere in string"
  :type 'boolean
  :group 'company-syntcl)

(defcustom company-syntcl--ignore-case nil
  "switch to control case-sensitive matching (default: \"off\" means case-sensitive matches)
set to \"on\" (t) to ignore case for candidate matches"
  :type 'boolean
  :group 'company-syntcl)


(defcustom company-syntcl--idle-delay nil
  "controls delay for idle matching
default (\"nil\") means *NO* idle triggering of NC completion.
This means you must explicitly trigger netlist completion.
Set a delay (in seconds) to auto-trigger Netlist completion.
NOTE: better to keep at default (nil) to avoid wrong completion type"
  :type '(choice
          (const :tag "never (nil)" nil)
          (number :tag "seconds"))
  :group 'company-syntcl)

(defcustom company-syntcl--force-read-json nil
  "force reading JSON files (default \"nil\")
set to \"on\" (t) to force (i.e. if JSON files change on updated database)"
  :type 'boolean
  :group 'company-syntcl)

(defcustom company-syntcl-align-annotation t
  "Align the annotation string so they are shown as their own column."
  :type 'boolean
  :group 'company-syntcl)

(defvar company-syntcl--cmd-max-len 0)  ; used to align annotations

(defvar company-syntcl--attr-flag "") ; will get non-empty value during *-ask-for-attr
(defvar company-syntcl--attr-class "")
(defvar company-syntcl--attr-type  "")

(defvar company-syntcl--active-cmd "")  ; will be set by *-find-active-cmd

(defvar company-syntcl--type nil)       ; default, set based on completion prefix...

;; these commands have no entries in ./TclComplete/details.vim (need to skip annotation)
(defvar company-syntcl--cmds-without-details
  '("array"
    "binary"
    "chan"
    "dict"
    "expr"
    "file"
    "info"
    "namespace"
    "package"
    "string"))

(defvar company-syntcl--last-completed-word "")

(defun company-syntcl--attr-completion ()
  "activate Tcl-completion for attributes"
  ;; set interactive so it can be bound to key
  (interactive)
  (setq company-syntcl--attr-flag "yes")
  (company-mode 1)
  (company-syntcl 'interactive))

;;;###autoload
(defun company-syntcl--tcl ()
  "activate TCL auto-completion"
  ;; set interactive so it can be bound to key
  (interactive)
  (company-mode 1)
  (company-syntcl 'interactive))

;; make byte-compiler happy!
;; list vars
(defvar tcl-commands-list nil)
(defvar tcl-namespaces-list nil)    ; added 2018/07/13
(defvar tcl-designs-list nil)
(defvar tcl-g_vars-list nil)
(defvar tcl-g_var_arrays-list nil)
(defvar tcl-iccpp-list nil)
(defvar tcl-techfile_types-list nil)
(defvar tcl-app_vars-list nil)      ; added 2018/06/28
(defvar tcl-regexp_char_classes-list nil) ; added 2018/07/12
(defvar tcl-packages-list nil)        ; added 2018/07/12
(defvar tcl-namespaces-list nil)      ; added 2018/07/12
;; hash tables
(defvar tcl-descriptions-hash nil)
(defvar tcl-options-hash nil)
(defvar tcl-app_options-hash nil)
(defvar tcl-environment-hash nil)
(defvar tcl-iccpp_dict-hash nil)
(defvar tcl-techfile_layer_dict-hash nil)
(defvar tcl-details-hash nil)
(defvar tcl-attributes-hash nil)
(defvar tcl-techfile_attr_dict-hash nil)
(defvar my-json-hash nil)
(defvar my-json-list nil)
(defvar tcl-track-pattern-list nil)         ; added 2018/10/08

;; added 2018/07/17 - check for member of list, rather than long regexes (easier maintenance)
(defvar tcl-attr-funcs
  '("get_attribute" "set_attribute" "filter_collection" "get_defined_attributes"))
(defvar tcl-gvar-funcs
  '("setvar" "getvar" "lappend_var" "info_var" "append_var"))
(defvar tcl-varname-funcs
  '("set" "unset" "append" "lappend" "lset" "incr" "dict set" "dict unset" "dict append" "dict lappend" "dict incr" "dict with" "dict update"))
(defvar tcl-env-funcs
  '("getenv" "setenv"))
(defvar tcl-design-funcs
  '("current_design" "set_working_design" "set_working_design_stack"))
(defvar tcl-app-opt-funcs
  '("get_app_options" "set_app_options" "report_app_options" "reset_app_options" "get_app_option_value"))

;; added 2018/10/08 - lists of colors, patterns, line styles for gui annotations
(defvar tcl-gui-colors '("white" "red" "orange" "yellow" "green" "blue" "purple" "light_red" "light_orange" "light_yellow" "light_green" "light_blue" "light_purple"))
(defvar tcl-gui-patterns '("BDDiagPattern" "CrossPattern" "Dense1Pattern" "Dense2Pattern" "Dense3Pattern" "Dense4Pattern" "Dense5Pattern" "Dense6Pattern" "Dense7Pattern" "DiagCrossPattern" "FDiagPattern" "kHorPattern" "NoBrush" "SolidPattern" "VerPattern"))
(defvar tcl-gui-types '("arrow" "line" "polygon" "polyline" "rectangle" "ruler" "symbol" "text"))
(defvar tcl-gui-symbol-types '("square" "triange" "diamond" "x"))
(defvar tcl-gui-linestyles '("CustomDashLine" "DashDotDotLine" "DashDotLine" "DashLine" "DotLine" "NoPen" "SolidLine"))
;; (defvar tcl-gui-symbol-sizes (number-sequence 3 100)) ; FROM: http://ergoemacs.org/emacs/elisp_list.html


;; required inputs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: all tclcomplete info is now stored in following JSON files:
;;
(defun company-syntcl--read-json-files ()
  "make hash tables for Tclcomplete data
* lists
   <tcl-complete-dir>/commands.json                    command
   <tcl-complete-dir>/designs.json                     subfc/ss/partition
   <tcl-complete-dir>/g_vars.json                      g_variables (all) but no arrays (i.e. 'G_AON_CONNECT_VIA_TABLE(' )
   <tcl-complete-dir>/g_var_arrays.json                g_variables with arrays (only)
   <tcl-complete-dir>/iccpp.json                       iccpp parameters
   <tcl-complete-dir>/techfile_types.json              techfile types
   <tcl-complete-dir>/app_vars.json                    app_vars
   <tcl-complete-dir>/regexp_char_classes.json         regexp character classes
   <tcl-complete-dir>/packages.json                    packages

* dictionary of lists -> (hash-table)                  key                     value
   <tcl-complete-dir>/descriptions.json                command                 description of command
   <tcl-complete-dir>/options.json                     command                 list of options
   <tcl-complete-dir>/app_options.json                 option                  type:default
   <tcl-complete-dir>/environment.json                 env_var                 value
   <tcl-complete-dir>/iccpp_dict.json                  iccpp param             default
   <tcl-complete-dir>/techfile_layer_dict.json         techfile_type           layer options

* dictionary of dictionaries -> (nested hash-table)    key                     value
   <tcl-complete-dir>/details.json                     [command][option]       description of option
   <tcl-complete-dir>/attributes.json                  [obj_class][attr_name]  choices
   <tcl-complete-dir>/techfile_attr_dict.json          ['type':'layer']        choices

NOTE: only read JSON file if *-<type>-hash variable NOT already set
"
  (let* ((json-object-type 'hash-table)
         (json-array-type  'list)
         (json-key-type    'string)
         (my-types '("list" "hash")))
    (message "company-syntcl: begin reading JSON files")
    (setq my-json-list '("commands"
                         "designs"
                         "g_vars"
                         "g_var_arrays"
                         "iccpp"
                         "app_vars"
                         "regexp_char_classes"
                         "packages"
                         "techfile_types"))
    (setq my-json-hash '("descriptions"
                         "options"
                         "app_options"
                         "environment"
                         "iccpp_dict"
                         "techfile_layer_dict"
                         "details"
                         "attributes"
                         "techfile_attr_dict"))
    (dolist (typ my-types)      ; loop thru 'my-types
      (dolist (fl (symbol-value (intern-soft (concat "my-json-" typ)))) ; loop thru *-list or *-hash
        (if (or (null (symbol-value (intern-soft (concat "tcl-" fl "-" typ))))
                company-syntcl--force-read-json)
            (if (file-exists-p (expand-file-name (concat fl ".json") company-syntcl-dir))
                (if (version< "26.1" emacs-version)
                    (set (intern-soft (concat "tcl-" fl "-" typ)) (json-read-file (expand-file-name (concat fl ".json") company-syntcl-dir)))
                  ;; else, need to test for "broken" JSON constructs in emacs 26.1 (i.e. embedded tabs)
                  (if (string-blank-p (shell-command-to-string (concat "grep -P '\".*\t.*\"' " (expand-file-name (concat fl ".json") company-syntcl-dir))))
                      (set (intern-soft (concat "tcl-" fl "-" typ)) (json-read-file (expand-file-name (concat fl ".json") company-syntcl-dir)))
                    ;; else, need to fix!
                    (message "File %s has embedded tabs, which breaks Emacs %s JSON parser" (concat fl ".json") emacs-version)
                    (with-temp-buffer
                      (insert-file-contents (expand-file-name (concat fl ".json") company-syntcl-dir))
                      (goto-char (point-min))
                      (while (search-forward "	" nil t) ; emacs 26 json parser does not like embedded tabs! (C-q <tab> to insert!)
                        (replace-match " " nil t))
                      (goto-char (point-min))
                      (set (intern-soft (concat "tcl-" fl "-" typ)) (json-read)))))
              (message "File %s does not exist in dir %s. Cannot perform Tcl completions." (concat fl ".json") company-syntcl-dir)))))
    (message "company-syntcl: end reading JSON files")
    ;; setting up derived variables
    (if (null tcl-namespaces-list)
        (setq tcl-namespaces-list (company-syntcl--get-namespaces)))
    (if (null tcl-track-pattern-list)
        (setq tcl-track-pattern-list (company-syntcl--get-track-patterns)))))

(defun company-syntcl--get-track-patterns ()
  "get list of track patterns from G_ROUTE_TRACK_PATTERNS var
NOTE: parses `tcl-g_var_array-list'"
  (let ((pattern-list)
        (pattern))
    (dolist (item tcl-g_var_arrays-list)
      (if (string-match "^G_ROUTE_TRACK_PATTERNS(" item)
          (progn
            (setq pattern (substring item (1+ (string-match "(" item)) (1- (length item))))
            (setq pattern-list (cons pattern pattern-list)))))
    (delete-dups pattern-list)))


;; prefix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-syntcl--find-active-cmd ()
  "find active command - either at first unmatched left bracket or beginning of line
NOTE: returns string"
  (interactive)
  (let* ((rt-bracket 0)
         (cur-pt (point))
         (char-behind)
         (bol (line-beginning-position))
         (str (buffer-substring-no-properties bol cur-pt))
         (indx (length str)))
    (save-excursion
      ;; continue going backward to start of line or first unmatched left bracket
      (while (and (> indx 0) (>= rt-bracket 0))
        (setq char-behind (substring str (1- indx) indx))
        (if (string-equal char-behind "]")
            (progn
              (setq rt-bracket (1+ rt-bracket)))
          (if (string-equal char-behind "[")
              (progn
                (setq rt-bracket (1- rt-bracket)))))
        (if (>= rt-bracket 0)      ; keep going back
            (setq indx (1- indx)))
        (backward-char))           ; at 1st unmatched '[' or line start
      ;; move index to right through whitespace until we hit command
      (while (and (< indx (length str)) (string-equal " " (substring str indx (1+ indx))))
        (setq indx (1+ indx)))
      ;; move index ahead of possible :: global namespace
      (if (string-equal "::" (substring str indx (let ((end (+ 2 indx)))
                                                   (if (< end (length str))
                                                       end (1+ indx)))))
          (setq indx (+ 2 indx)))
      (string-match "\\([\\-a-zA-Z0-9:_]+\\)" str indx)  ; here is active command
      (setq company-syntcl--active-cmd (match-string-no-properties 1 str))
      ;; substract 1 from length of str, then do split-string, then take 2nd to last element from list
      ;; string                   last-completed-word
      ;; "ab cd ef g"             "ef"
      ;; "ab cd ef "              "ef"
      (setq company-syntcl--last-completed-word
            (nth (1- (length (split-string (substring str 0 (1- (string-width str))))))
                 (split-string (substring str 0 (1- (string-width str)))))))
    company-syntcl--active-cmd))

(defun company-syntcl--prefix ()
  "grabs completion string prefix using `company-grab-symbol.
Tries to determine type of completion (variable, command, attribute, option)
and sets variable `company-syntcl--type'
NOTE: returns string"
  (let ((prfx (company-grab-symbol)))
    (setq company-syntcl--type "")  ; initialize each time `prefix' called
    (company-syntcl--find-active-cmd)    ; sets *--active-cmd, *--last-completed-word
    ;; need to strip off leading '[' from prfx (if present)
    ;; ... because '[',']' added to tcl symbol table to enable sending indexed strings to tmux icc2 shell
    ;; ... ditto for '('
    (cond
     ((eq ?\[ (string-to-char prfx)) (setq prfx (substring prfx 1)))
     ((eq ?\( (string-to-char prfx)) (setq prfx (substring prfx 1))))
    (cond                                ; set vars based upon *--active-cmd
     ((string-equal company-syntcl--active-cmd prfx)  (setq company-syntcl--active-cmd ""))) ;; NOTE: only set *--attr-flag if need to prompt for attribute class
    (if (not (string-equal "" prfx))
        ;; string-to-char returns first char
        (cond
         ((eq ?$ (string-to-char prfx))            (setq company-syntcl--type "var-names")) ; variables in file
         ((eq ?- (string-to-char prfx))            (setq company-syntcl--type "options")) ; '-foo'
         ((string-match "^G_" prfx)                (setq company-syntcl--type "gvars")) ; G_vars
         ((and (string-match "^:" prfx) ; regexp char classes start with single colon ([:alnum:], [:upper:], etc)
               (not (string-match ":" (substring prfx 1 2))))
          (setq company-syntcl--type "regexp-char-classes"))
         ((string-equal "" company-syntcl--active-cmd)   (setq company-syntcl--type "commands")) ; command
         ((string-equal "yes" company-syntcl--attr-flag) (setq company-syntcl--type "attributes")) ; attribute
         ((member company-syntcl--active-cmd tcl-attr-funcs) (setq company-syntcl--type "attributes")) ; attribute
         (t
          (setq company-syntcl--type "options"))) ; default   ; end cond3
      ;; else (prfx = "")
      ;; have to account for multiple completions on single line
      ;; i.e. 'get_' -> 'get_cells -' -> 'get_cells -design ' -> 'get_cells -design sprxccsouth01' ...
      (cond
       ;; NOTE: *-attr-flag can be "yes" when 'prfx' is ""
       ((string-equal "yes" company-syntcl--attr-flag) (setq company-syntcl--type "attributes")) ; attribute
       ((member company-syntcl--active-cmd tcl-attr-funcs) (setq company-syntcl--type "attributes")) ; attribute
       ((and (string-match "^get_" company-syntcl--active-cmd)
             (string-equal "-filter" company-syntcl--last-completed-word))
        (setq company-syntcl--type "attributes"))
       ((member company-syntcl--active-cmd tcl-gvar-funcs) (setq company-syntcl--type "gvars"))
       ((or (member company-syntcl--active-cmd tcl-varname-funcs)
            (member (concat company-syntcl--active-cmd " " company-syntcl--last-completed-word) tcl-varname-funcs))
        (setq company-syntcl--type "var-names"))
       ((string-match "\\(iccpp_com\\:\\:set_param\\|iccpp_com\\:\\:get_param\\)" company-syntcl--active-cmd) ;iccpp
        (setq company-syntcl--type "iccpp"))
       ((member company-syntcl--active-cmd tcl-app-opt-funcs) (setq company-syntcl--type "app-options"))
       ((or (string-equal "-design" company-syntcl--last-completed-word)
            (member company-syntcl--active-cmd tcl-design-funcs))
        (setq company-syntcl--type "designs"))
       ((member company-syntcl--active-cmd tcl-env-funcs) (setq company-syntcl--type "env-vars"))
       ((string-match "app_var" company-syntcl--active-cmd)
        (setq company-syntcl--type "app-vars"))
       ;; check for various 2 word combinations
       ((and (string-match "namespace" company-syntcl--active-cmd)
             (not (string-match "namespace" company-syntcl--last-completed-word)))
        (setq company-syntcl--type "namespaces"))
       ((and (string-match "package" company-syntcl--active-cmd)
             (not (string-match "package" company-syntcl--last-completed-word)))
        (setq company-syntcl--type "packages"))
       ((or (string-match "parray" company-syntcl--active-cmd)
            (and (string-match "array" company-syntcl--active-cmd)
                 (not (string-match "array" company-syntcl--last-completed-word))))
        (setq company-syntcl--type "array-names"))
       ((ht-contains-p tcl-options-hash (concat company-syntcl--active-cmd " " company-syntcl--last-completed-word))
        (setq company-syntcl--type "two-word-opt"))
       ;; techfile stuff
       ((string-match "tech\\:\\:get_techfile_info" company-syntcl--active-cmd)
        (setq company-syntcl--type "techfile"))
       ;; track pattern completion
       ((and (string-match "cr_create_track_region" company-syntcl--active-cmd)
             (string-match "-pattern" company-syntcl--last-completed-word))
        (setq company-syntcl--type "track-patterns"))
       ;; gui completions
       ((and (string-match "^gui" company-syntcl--active-cmd)
             (not (string-match "^gui" company-syntcl--last-completed-word)))
        (setq company-syntcl--type "gui"))
       (t
        (setq company-syntcl--type "options"))))
    (if (string-prefix-p "-" prfx)
        (cons prfx t)
      prfx)))


;; candidate parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-syntcl--scan-buffer-for-var-names (prefix var-type)
  "scan whole buffer for variable names"
  (interactive)
  (let ((regex)
        (re-cmt "^[[:space:]]*#")
        (re-proc "proc .*{\\(.*\\)}[[:space:]]+{$")
        (var-names '())
        (var)
        (var-proc '())
        (var-list '())
        (var-name-complt '())
        (cur-point (point)))
    (if (string-equal "var" var-type)
        (setq regex (concat
                     "\\("
                     "\\$"
                     "\\|foreach[[:space:]]+"
                     "\\|foreach_in_collection[[:space:]]+"
                     "\\|set[[:space:]]+"
                     "\\)"
                     "\\([a-zA-Z0-9_]+\\)[[:space:]]*"))
      (setq regex (concat
                   "\\("
                   "array[[:space:]]+set[[:space:]]+"
                   "\\)"
                   "\\([a-zA-Z0-9_]+\\)[[:space:]]*")))
    (save-excursion
      (goto-char (point-min))       ; go to beginning of buffer
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (myline (buffer-substring-no-properties bol eol)))
          (goto-char bol)
          (if (or (and (>= cur-point bol) (<= cur-point eol))
                  (string-match re-cmt myline)
                  (string-match "^[[:space:]]*$" myline))
              (progn
                (forward-line))     ; skip current line ...
            ;; else ...
            ;; loop to get all vars in line
            (while (not (eolp))
              (if (re-search-forward regex (line-end-position) t)
                  (progn
                    (if (string-equal "$" prefix) ; need to add leading '$'
                        (setq var (concat "$" (match-string-no-properties 2)))
                      (setq var (match-string-no-properties 2))) ; do not add leading '$'
                    (if (null (member var var-names))
                        (setq var-names (cons var var-names))))
                (forward-char)))
            ;; check for proc args or scan|lassign vars if var-type = "var"
            (if (string-equal "var" var-type)
                (progn
                  ;; check for proc arguments
                  (goto-char bol)
                  (if (re-search-forward re-proc (line-end-position) t)
                      (progn
                        (setq var-proc (split-string (match-string-no-properties 1) " "))
                        (dolist (item var-proc)
                          (if (and (null (member item var-names))
                                   (not (string-match "}" item))
                                   (not (string-match "^[[:space:]]*$" item)))
                              (setq var-names (cons item var-names))))))
                  ;; Check for final args of lassign or scan (go backwards from end. look for valid var names)
                  (if (string-match "\\(lassign\\|scan\\)" myline)
                      (progn
                        (setq var-list (reverse (split-string myline " ")))
                        (dolist (item var-list)
                          (if (and (not (string-match "[]%$}\"\[]" item))
                                   (not (string-match "^[[:space:]]*$" item))
                                   (not (string-match "\\(scan\\|lassign\\)" item)))
                              (progn
                                (if (null (member item var-names))
                                    (setq var-names (cons item var-names))))))))))
            (forward-line)))))
    (dolist (item var-names)
      (if (or (eq 0 (string-match prefix item))
              (string-equal "$" prefix))
          (setq var-name-complt (cons item var-name-complt))))
    (delete-dups var-name-complt)))

(defun company-syntcl--get-namespaces ()
  "finds all namespaces in commands.json, which will be saved in variable `tcl-namespaces-list'
NOTE: returns list"
  (let ((namespaces '())
        (regex "\\(::[^:]*$\\)")
        (base))
    (dolist (item tcl-commands-list)
      (if (string-match "::" item)
          (progn
            (setq base (replace-regexp-in-string regex "" item))
            (if (null (member base namespaces))
                (setq namespaces (cons base namespaces))))))
    (delete-dups namespaces)))

(defun company-syntcl--ask-for-attr-class (attr-class)
  "ask user to specify attribute class, will return attribute candidate list based upon attr-class
NOTE: returns list"
  (let ((attr-list '()))
    (if (string-blank-p attr-class)
        (progn
          (setq company-syntcl--attr-type "class")
          (message "Listing attribute classes ...")
          (setq attr-list (ht-keys tcl-attributes-hash)))
      ;; else
      (if (y-or-n-p (format "Current attribute class is <%s>. Enter 'y' to keep, 'n' to change." company-syntcl--attr-class))
          (progn
            (setq company-syntcl--attr-type "option")
            (message "Listing options for attribute type <%s>" company-syntcl--attr-class)
            (setq attr-list (ht-keys (ht-get tcl-attributes-hash company-syntcl--attr-class))))
        ;; else
        (setq company-syntcl--attr-type "class")
        (setq attr-list (ht-keys tcl-attributes-hash))))
    (delete-dups attr-list)))

(defun company-syntcl--set-attr-class (match)
  "sets variable `company-syntcl--attr-class', which will be used to select relevant options for attribute commands
This function is called as a post-completion hook, i.e. will complete attr-class, then will remove attr-class
from line, and next completion will complete appropriate attribute
NOTE: sets variable"
  (search-backward match)
  (goto-char (match-beginning 0))
  (replace-match "")
  (setq company-syntcl--attr-class match)
  (message "... Setting attribute class to <%s>" company-syntcl--attr-class))

(defun company-syntcl--get-obj-class (get-cmd)
  "try to automatically determine *-attr-class from *-active-cmd name
special processing for active-cmd = 'get_XXXXX' and last-completed-word = '-filter'
NOTE: returns string"
  (let ((obj (substring get-cmd 4))
        (res ""))
    (if (ht-contains-p tcl-attributes-hash obj)
        (setq res obj)
      ;; try stripping off trailing 's'
      (if (ht-contains-p tcl-attributes-hash (substring obj 0 (1- (string-width obj))))
          (setq res (substring obj 0 (1- (string-width obj))))))
    res))

(defun company-syntcl--attr-candidates (prfx)
  "autocomplete function for attributes
Original source: ./TclComplete/attributes.json
NOTE: returns a list of candidates"
  (let* ((bol (line-beginning-position))
         (eol (point))
         (myline (buffer-substring-no-properties bol eol))
         (attr-obj-list '())
         (attr-complete-opts '())
         (split-prfx "")
         (split-line "")
         (base "")
         (idx))
    ;; first, check for '-class' (usually during a 'get_defined_attributes' type command)
    (if (string-match "-class" company-syntcl--last-completed-word)
        (progn
          (setq attr-obj-list (ht-keys tcl-attributes-hash)))
      ;; else
      (if (string-match "yes" company-syntcl--attr-flag) ; explicitly triggered by user (keybinding)
          (progn
            (setq attr-obj-list (company-syntcl--ask-for-attr-class company-syntcl--attr-class)))
        ;; else
        (if (string-match "\\." prfx)   ; dotted attribute format (like cell.name, pin.cell.origin)
            (progn
              (setq split-prfx (split-string prfx "\\."))
              ;; prfx            split-prfx
              ;; "cell."         ("cell" "")       - if prfx ends in ".", last element is ""
              ;; "cell.name"     ("cell" "name")
              (setq company-syntcl--attr-class (car (last (butlast split-prfx)))) ; gets 2nd-to-last list element
              (if (string-match "\\.$" prfx) ; dotted attribute ends with "."
                  (setq base prfx)
                (setq base (concat (mapconcat 'identity (butlast split-prfx) ".") ".")))
              ;; prepend all attr completion candidates with base (i.e. "cell."candidate)
              (setq attr-obj-list (mapcar (lambda (c) (concat base c))
                                          (ht-keys (ht-get tcl-attributes-hash company-syntcl--attr-class)))))
          ;; else (no '.' in prfx)
          ;; attribute completion after '-filter' option of get_cells, get_nets, etc.
          (if (and (string-match "^get_" company-syntcl--active-cmd)
                   (string-equal "-filter" company-syntcl--last-completed-word))
              (progn
                (setq company-syntcl--attr-class (company-syntcl--get-obj-class company-syntcl--active-cmd))
                (setq attr-obj-list (ht-keys (ht-get tcl-attributes-hash company-syntcl--attr-class))))
            ;; else
            ;; try to derive object class based upon '-class' argument
            (if (string-match "-class" myline)
                (progn
                  (setq split-line (split-string myline))
                  (setq idx (1+ (cl-position "-class" split-line :test 'equal)))
                  (setq company-syntcl--attr-class (nth idx split-line))
                  (setq attr-obj-list (ht-keys (ht-get tcl-attributes-hash company-syntcl--attr-class)))) ;; else
              ;; need to ask for attribute
              (setq attr-obj-list (company-syntcl--ask-for-attr-class company-syntcl--attr-class)))))))

    (dolist (item attr-obj-list)
      (if company-syntcl--prefix-start
          (progn
            (if (eq 0 (string-match prfx item))
                (progn
                  (if (> (length item) company-syntcl--cmd-max-len)
                      (setq company-syntcl--cmd-max-len (length item)))
                  (setq attr-complete-opts (cons item attr-complete-opts)))))
        (if (string-match prfx item)
            (progn
              (if (> (length item) company-syntcl--cmd-max-len)
                  (setq company-syntcl--cmd-max-len (length item)))
              (setq attr-complete-opts (cons item attr-complete-opts))))))
    (delete-dups attr-complete-opts)))

(defun company-syntcl--gui-candidates (prfx)
  "Candidates for gui_annotation command (color,pattern,linestyles,symbol-type)
NOTE: returns list of candidates"
  (let ((cand-obj-list '())
        (cand-complete-opts '()))
    (cond
     ((string-equal "-color" company-syntcl--last-completed-word)
      (setq cand-obj-list tcl-gui-colors))
     ((string-equal "-pattern" company-syntcl--last-completed-word)
      (setq cand-obj-list tcl-gui-patterns))
     ((string-equal "-symbol_type" company-syntcl--last-completed-word)
      (setq cand-obj-list tcl-gui-symbol-types))
     ((string-equal "-type" company-syntcl--last-completed-word)
      (setq cand-obj-list tcl-gui-types))
     ((string-equal "-line_style" company-syntcl--last-completed-word)
      (setq cand-obj-list tcl-gui-linestyles)))
    (dolist (element cand-obj-list cand-complete-opts)
      (if company-syntcl--prefix-start
          (progn
            (if (eq 0 (string-match prfx element))
                (progn
                  (if (> (length element) company-syntcl--cmd-max-len)
                      (setq company-syntcl--cmd-max-len (length element)))
                  (setq cand-complete-opts (cons element cand-complete-opts)))))
        (if (string-match prfx element)
            (progn
              (if (> (length element) company-syntcl--cmd-max-len)
                  (setq company-syntcl--cmd-max-len (length element)))
              (setq cand-complete-opts (cons element cand-complete-opts))))))
    (delete-dups cand-complete-opts)))

(defun company-syntcl--get-candidate-list (prfx obj-type)
  "Generic AutoComplete function to get completion candidates based upon `obj-type'
NOTE: returns list of candidates"
  (let ((case-fold-search company-syntcl--ignore-case)
        (cand-obj-list '())
        (cand-complete-opts '()))
    (cond
     ((string-equal "commands" obj-type) (setq cand-obj-list tcl-commands-list))
     ((string-equal "designs"  obj-type) (setq cand-obj-list tcl-designs-list))
     ((and (string-equal "gvars" obj-type) ;check for g_var arrays first
           (string-match "(" prfx))
      (setq cand-obj-list tcl-g_var_arrays-list))
     ((string-equal "gvars"        obj-type) (setq cand-obj-list tcl-g_vars-list))
     ((string-equal "app-vars"     obj-type) (setq cand-obj-list tcl-app_vars-list))
     ((string-equal "namespaces"   obj-type) (setq cand-obj-list tcl-namespaces-list))
     ((string-equal "packages"     obj-type) (setq cand-obj-list tcl-packages-list))
     ((string-equal "options"      obj-type) (setq cand-obj-list (ht-get  tcl-options-hash company-syntcl--active-cmd)))
     ((string-equal "app-options"  obj-type) (setq cand-obj-list (ht-keys tcl-app_options-hash )))
     ((string-equal "env-vars"     obj-type) (setq cand-obj-list (ht-keys tcl-environment-hash )))
     ((string-equal "iccpp"        obj-type) (setq cand-obj-list (ht-keys tcl-iccpp_dict-hash )))
     ((string-equal "two-word-opt" obj-type) (setq cand-obj-list (ht-get tcl-options-hash (concat company-syntcl--active-cmd " " company-syntcl--last-completed-word))))
     ((string-equal "track-patterns"   obj-type) (setq cand-obj-list tcl-track-pattern-list))
     )
    (dolist (element cand-obj-list cand-complete-opts)
      (if company-syntcl--prefix-start
          (progn
            (if (eq 0 (string-match prfx element))
                (progn
                  (if (> (length element) company-syntcl--cmd-max-len)
                      (setq company-syntcl--cmd-max-len (length element)))
                  (setq cand-complete-opts (cons element cand-complete-opts)))))
        (if (string-match prfx element)
            (progn
              (if (> (length element) company-syntcl--cmd-max-len)
                  (setq company-syntcl--cmd-max-len (length element)))
              (setq cand-complete-opts (cons element cand-complete-opts))))))
    (delete-dups cand-complete-opts)))

(defun company-syntcl--parse-tf-args (opt)
  "parse '-option' value
NOTE: returns string or empty string"
  (interactive)
  (save-excursion
    (let* ((bol (line-beginning-position))
           (pt  (point))
           (str (buffer-substring-no-properties bol pt))
           (val ""))
      (if (string-match (concat opt " \\([[:alnum:]]+\\) ") str)
          (setq val (match-string-no-properties 1 str)))
      val)))

(defun company-syntcl--techfile-candidates (prfx)
  "AutoComplete function for 'tech::get_techfile_info'
Original source: ./TclComplete/techfile_types.json, techfile_attr_dict.json, techfile_layer_dict.json
NOTE: returns list of candidates"
  (let ((case-fold-search company-syntcl--ignore-case)
        (techfile-obj-list '())
        (techfile-complete-opts '())
        (tf-type "")
        (tf-layer ""))
    ;; first, check to see if no options entered (prfx = "")
    (if (and (string-equal "" prfx)
             (string-equal company-syntcl--active-cmd company-syntcl--last-completed-word)) ; "tech::get_techfile_info "
        (setq techfile-obj-list (ht-get tcl-options-hash company-syntcl--active-cmd))
      ;; else
      (if (string-equal "-type" company-syntcl--last-completed-word)
          (setq techfile-obj-list tcl-techfile_types-list)
        (if (string-equal "-layer" company-syntcl--last-completed-word)
            (progn
              (setq tf-type (company-syntcl--parse-tf-args "-type"))
              (setq techfile-obj-list (ht-get tcl-techfile_layer_dict-hash tf-type)))
          ;; else
          (setq tf-type  (company-syntcl--parse-tf-args "-type"))
          (setq tf-layer (company-syntcl--parse-tf-args "-layer"))
          (setq techfile-obj-list (ht-get tcl-techfile_attr_dict-hash (concat tf-type ":" tf-layer))))))
    (dolist (element techfile-obj-list techfile-complete-opts)
      (if company-syntcl--prefix-start
          (progn
            (if (eq 0 (string-match prfx element))
                (progn
                  (if (> (length element) company-syntcl--cmd-max-len)
                      (setq company-syntcl--cmd-max-len (length element)))
                  (setq techfile-complete-opts (cons element techfile-complete-opts)))))
        (if (string-match prfx element)
            (progn
              (if (> (length element) company-syntcl--cmd-max-len)
                  (setq company-syntcl--cmd-max-len (length element)))
              (setq techfile-complete-opts (cons element techfile-complete-opts))))))
    (delete-dups techfile-complete-opts)))

(defun company-syntcl--candidates (prefix obj-type)
  "Processes auto-complete prefix to generate completion candidates.
Calls specific functions to get candidates based upon `obj-type'
NOTE: returns list of completion candidates"
  (let ((company-syntcl--prfx prefix)
        (obj-complete-opts '()))
    (setq company-syntcl--cmd-max-len 0)  ; reset here
    (cond
     ((string-equal obj-type "var-names")
      (setq obj-complete-opts (company-syntcl--scan-buffer-for-var-names company-syntcl--prfx "var")))
     ((string-equal obj-type "array-names")
      (setq obj-complete-opts (company-syntcl--scan-buffer-for-var-names company-syntcl--prfx "array")))
     ((string-equal obj-type "attributes")
      (setq obj-complete-opts (company-syntcl--attr-candidates company-syntcl--prfx)))
     ((string-equal obj-type "techfile")
      (setq obj-complete-opts (company-syntcl--techfile-candidates company-syntcl--prfx)))
     ((string-equal obj-type "gui")
      (setq obj-complete-opts (company-syntcl--gui-candidates company-syntcl--prfx)))
     ;; all other `obj-type' complete here
     (t
      (setq obj-complete-opts (company-syntcl--get-candidate-list company-syntcl--prfx obj-type))))
    (sort obj-complete-opts 'string<)))


;; annotation  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-syntcl--annotation (candidate)
  "annotation for TCL completion candidates
NOTE: returns annotation *string* for each candidate"
  (when-let
      ((anno
        (pcase company-syntcl--type
          ("commands" (ht-get tcl-descriptions-hash candidate))
          ("iccpp" (ht-get tcl-iccpp_dict-hash candidate))
          ("env-vars" (ht-get tcl-environment-hash candidate))
          ("app-options" (ht-get tcl-app_options-hash candidate))
          ("options" (when (not (or (member company-syntcl--active-cmd
                                            company-syntcl--cmds-without-details)
                                    (equal "package require" company-syntcl--active-cmd)))
                       (ht-get* tcl-details-hash company-syntcl--active-cmd candidate)))
          ("attributes" (unless (equal company-syntcl--attr-type
                                       "class")
                          (ht-get* tcl-attributes-hash company-syntcl--attr-class candidate)))
          ("techfile" (when (equal company-syntcl--active-cmd
                                   company-syntcl--last-completed-word)
                        (ht-get* tcl-details-hash company-syntcl--active-cmd candidate))))))
    (unless (string-blank-p anno)
      (concat (s-repeat (if company-syntcl-align-annotation
                            (- (+ 3 company-syntcl--cmd-max-len)
                               (length candidate))
                          3) " ")
              anno))))



;; setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun company-syntcl--setup ()
  "Instructions to setup Tcl Completion:
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
")


;; misc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no longer needed since tcl symbol table *NOT* modified?
(defun company-syntcl--get-xref ()
  "wrapper for `xref-find-definitions' because '[',']','(',')' added to tcl-mode symbol table"
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    ;; need to strip off leading '[','('
    (cond
     ((eq ?\[ (string-to-char thing)) (setq thing (substring thing 1)))
     ((eq ?\( (string-to-char thing)) (setq thing (substring thing 1))))
    ;; ... and trailing ']',')'
    (cond
     ((string-match "]$" thing)       (setq thing (substring thing 0 (1- (string-width thing)))))
     ((string-match ")$" thing)       (setq thing (substring thing 0 (1- (string-width thing))))))
    (xref-find-definitions thing)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun company-syntcl (command &optional arg &rest ignored)
  "`company-mode' completion backend for TclComplete.
this company backend provides completion for Synopsys Tcl commands

this is based upon Chris Heithoffs's TclComplete Vim plugin
/nfs/pdx/home/cbheitho/MyVimPlugins/TclComplete

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

 <cntl>-c t    Synopsys Tcl completion

 <alt>-v          scroll previous page of completions
 <cntl>-v         scroll next page of completions
 <cntl>-s         search forward in completions
 <cntl>-r         search reverse in completions
 <cntl>-<alt>-s   narrow completion candidates to search string

** Customization options **

there are some customize options available to control how Tcl Complete mode works

type \"M-x customize-group RET company-syntcl RET\" to get list of user-specified options
"
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-syntcl))
    (init (when (memq major-mode company-syntcl-modes)
            (company-syntcl--read-json-files)))
    (prefix (and (memq major-mode company-syntcl-modes)
                 (or (company-syntcl--prefix) 'stop)))
    (candidates (company-syntcl--candidates arg company-syntcl--type))
    (annotation (company-syntcl--annotation arg))
    (post-completion
     (cond
      ((and (string-equal company-syntcl--type "attributes")
            (string-equal company-syntcl--attr-type "class"))
       (company-syntcl--set-attr-class arg)) ; 'arg' is the completed text
      ((string-equal company-syntcl--type "attributes") (setq company-syntcl--attr-flag "no"))))
    (no-cache t)
    (ignore-case company-syntcl--ignore-case)
    (sorted t)))

(provide 'company-syntcl)
;;; company-syntcl.el ends here

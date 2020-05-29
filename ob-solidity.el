;;; ob-solidity.el --- org-babel functions for solidity evaluation

;; Copyright (C) your name here

;; Author: your name here
;; Keywords: literate programming, reproducible research
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is not intended to ever be loaded by org-babel, rather it
;; is a solidity for use in adding new language support to Org-babel.
;; Good first steps are to copy this file to a file named by the
;; language you are adding, and then use `query-replace' to replace
;; all strings of "solidity" in this file with the name of your new
;; language.
;;
;; If you have questions as to any of the portions of the file defined
;; below please look to existing language support for guidance.
;;
;; If you are planning on adding a language to org-babel we would ask
;; that if possible you fill out the FSF copyright assignment form
;; available at https://orgmode.org/request-assign-future.txt as this
;; will make it possible to include your language support in the core
;; of Org-mode, otherwise unassigned language support files can still
;; be included in the contrib/ directory of the Org-mode repository.

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("sol" . "tmp"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:solidity '())

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:solidity' function below.
(defun org-babel-expand-body:solidity (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  ;; (require 'inf-solidity)
  ;; (let ((vars (nth 1 (or processed-params (org-babel-process-params params)))))
  ;;   (concat
  ;;    (mapconcat ;; define any variables
  ;;     (lambda (pair)
  ;;       (format "%s=%S"
  ;;               (car pair) (org-babel-solidity-var-to-solidity (cdr pair))))
  ;;     vars "\n") "\n" body "\n"))
  body
  )

(defun org-babel-eval-error-notify-solidity (exit-code stderr)
  "Open a buffer to display STDERR and a message with the value of EXIT-CODE."
  (let ((buf (get-buffer-create org-babel-error-buffer-name)))
	(progn
	  (with-current-buffer buf (goto-char (point-max))
						   (save-excursion (insert stderr)))
	  (with-current-buffer buf (buffer-string)))))



(defun org-babel-eval-solidity (cmd body)
  "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
  (let ((err-buff (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer err-buff (erase-buffer))
    (with-temp-buffer
      (insert body)
      (setq exit-code
			(org-babel--shell-command-on-region
			 (point-min) (point-max) cmd err-buff))
      (if (or (not (numberp exit-code)) (> exit-code 0))
		  (with-current-buffer err-buff (buffer-string))
		  ;; (progn
		  ;; 	(with-current-buffer err-buff
		  ;; 	  (progn
		  ;; 		(print (buffer-string))
		  ;; 		(org-babel-eval-error-notify exit-code (buffer-string)))
		  ;; 	  )
		  ;; 	(save-excursion
		  ;; 	  (when (get-buffer org-babel-error-buffer-name)
		  ;; 		(with-current-buffer org-babel-error-buffer-name
		  ;; 		  (unless (derived-mode-p 'compilation-mode)
		  ;; 			(compilation-mode))
		  ;; ;; Compilation-mode enforces read-only, but Babel expects the buffer modifiable.
		  ;; 		  (setq buffer-read-only nil))))
		  ;; 	nil)
		(buffer-string)))))




;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:solidity (body params)
  "Execute a block of Solidity code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Solidity source code block.")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the session variable is non-nil
         ;; (session (org-babel-solidity-initiate-session (first processed-params)))
         ;; variables assigned for use in the block
         (vars (second processed-params))
         (result-params (third processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (fourth processed-params))
         ;; expand the body with `org-babel-expand-body:solidity'
         (full-body (org-babel-expand-body:solidity
                     body params processed-params)))

	(message "Printing")
	(print processed-params)
	(org-babel-eval-solidity "/home/harikrishnan/area51/solidity/Debug/solc/solc --asm - " full-body)
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;;
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    ))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:solidity (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-solidity-var-to-solidity (var)
  "Convert an elisp var into a string of solidity source code
specifying a var of the same value."
  (message "got here in solidity-var-to-solidity")
  (format "%S" var))

(defun org-babel-solidity-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-solidity-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-solidity)
;;; ob-solidity.el ends here

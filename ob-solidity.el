;;; ob-solidity.el --- org-babel functions for solidity evaluation

;; Copyright (C) Harikrishnan Mulackal

;; Author: Harikrishnan Mulackal
;; Keywords: solidity, literate programming, reproducible research
;; Homepage: https://github.com/hrkrshnn/ob-solidity
;; Version: 0.01

;;; License: GNU GPL-v3

;;; Requirements: solidity-mode https://github.com/ethereum/emacs-solidity

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
(require 'solidity-mode)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("solidity" . "sol"))

;; This function expands the body of a source code block by doing
;; things like prepending argument definitions to the body, it should
;; be called by the `org-babel-execute:solidity' function below.
(defun org-babel-expand-body:solidity (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  body
  )

;; Modification of `org-babel-eval' that will show the error message in the same buffer
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
		(buffer-string)))))

;; This is the main function which is called to evaluate a code
;; block.
(defun org-babel-execute:solidity (body params)
  "Execute a block of Solidity code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Solidity source code block.")
  (let* ((processed-params (org-babel-process-params params))
		 (full-body (org-babel-expand-body:solidity
					 body params processed-params))
		 (args (cdr (assoc :args processed-params))))

	(org-babel-eval-solidity (concat solidity-solc-path " " args " - ") full-body)))

(provide 'ob-solidity)
;;; ob-solidity.el ends here

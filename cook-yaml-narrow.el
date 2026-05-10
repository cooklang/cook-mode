;;; cook-yaml-narrow.el --- yaml narrowing within cook-mode metadata -*- lexical-binding: t; -*-

;; Author: Andrew Chi <chifamicom@outlook.com>
;; Created: 10 May 2026
;; Version: 0.0.1
;; Keywords: languages, convenience
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This would enable editing yaml without having to throw in an entire yaml
;; parser.  The end user may also have specialized key bindings in a yaml
;; major-mode that would not appear in `cook-mode', giving them a better
;; experience editing the buffer.
;;
;; This module works on `cook-mode' and `cook-ts-mode'.

;;; Code:

(defgroup cook-yaml-narrow '()
  "Yaml narrowing for `cook-mode' metadata."
  :group 'cook
  :prefix "cook-yaml-narrow-")

(defcustom cook-yaml-narrow-default-yaml-major-mode (cond
                                                     ;; yaml-mode is not provided with Emacs
                                                     ((fboundp 'yaml-mode)
                                                      'yaml-mode)
                                                     ;; people may be trying to avoid tree-sitter but this is a good default
                                                     ((treesit-language-available-p 'yaml)
                                                      #'yaml-ts-mode)
                                                     (t
                                                      #'fundamental-mode))
  "The major-mode for yaml when narrowed without tree-sitter."
  :group 'cook-yaml-narrow
  :type 'function)

(defcustom cook-yaml-narrow-inhibit-message-p nil
  "Whether to remove the message reminding you how to widen the buffer."
  :group 'cook-yaml-narrow
  :type 'boolean)

(defun cook-yaml-narrow-pick-major-mode ()
  "Pick a major mode to handle yaml with.

The choice is based on the current major mode."
  (if (derived-mode-p 'cook-mode)
      (funcall cook-yaml-narrow-default-yaml-major-mode)
    (yaml-ts-mode)))

(defun cook-yaml-narrow-widen ()
  "`widen' the current buffer and go back to `cook-mode'."
  (interactive)
  (widen)
  (if (derived-mode-p 'yaml-ts-mode)
      (progn
        (eval-and-compile (require 'cook-ts-mode))
        (cook-ts-mode))
    (eval-and-compile (require 'cook-mode))
    (cook-mode))

  (keymap-local-unset "<remap> <widen>"))

(defun cook-yaml-narrow ()
  "Narrow to only the yaml metadata section."
  (interactive)
  (let ((start (save-excursion
                 (re-search-backward "^---$")
                 (forward-line)
                 (point)))
        (end (save-excursion
               (re-search-forward "^---$")
               (beginning-of-line 0)
               (point))))
    (narrow-to-region start end)
    (cook-yaml-narrow-pick-major-mode)

    (keymap-local-set "<remap> <widen>" #'cook-yaml-narrow-widen)

    (unless cook-yaml-narrow-inhibit-message-p
      (message "Widen the buffer with `C-x n w'."))))

(provide 'cook-yaml-narrow)

;;; cook-yaml-narrow.el ends here
